library(data.table)
library(dplyr)
library(ggplot2)
library(magrittr)
library(stringr)


source("./utils.R")


teams <- paste("team", c(1:50), sep = "_")
team_scores <- rnorm(50, mean=6, sd=1.4) %>% pmin(10) %>% sort(decreasing = T)
teams_dt <- data.table(team=teams,team_score=team_scores)


primary_attributes <- c("A","B","C")
primary_attribute_scores <- teams_dt[,lapply(team_score, 
                                             split_team_score, 
                                             split_count=3,
                                             max_score=10) %>% unlist]

dimensions_dt <- teams_dt[rep(1:.N, each=length(primary_attributes))]
dimensions_dt[,`:=` (dimension=rep(primary_attributes, uniqueN(team)),
                   dimension_score=primary_attribute_scores)]

# ggplot(dimensions_dt, aes(x=dimension_score, fill=dimension)) + geom_density(aes(alpha=.5))
# ggplot(dimensions_dt, aes(x=dimension_score-team_score, fill=dimension)) + geom_density(aes(alpha=.5)) + xlim(-10,10)

matchups_dt <- dimensions_dt[rep(c(1:.N),each=length(teams)),
                            .(home_team=team, away_team=rep(teams, nrow(dimensions_dt)), 
                              dimension, home_score=dimension_score,
                              away_score=dimensions_dt[teams_dt, on="team"][order(dimension),rep(dimension_score, length(teams))])]

# QA Matchups

team_38_home <- matchups_dt[home_team=="team_38" & dimension=="B", home_score]
team_38_away <- matchups_dt[away_team=="team_38" & dimension=="B", away_score]
team_10_home <- matchups_dt[home_team=="team_10" & dimension=="A", home_score]
team_10_away <- matchups_dt[away_team=="team_10" & dimension=="A", away_score]

if (team_38_home!=team_38_away | team_10_home!=team_10_away) {
  stop("Home ability scores should match away ability scores. 
       Check matchups join")
}

# Set score margin
matchups_dt[,score_margin:=home_score-away_score]

# Remove duplicate matchups

matchups_dt[, rbind(home_team, away_team, dimension)] %>% 
  apply(2,function(x) {x %>% sort %>% paste(collapse=",")}) %>% 
  {.} -> matchup_keys

matchups_dt[,matchup_key:=matchup_keys]
matchups_dt <- matchups_dt[!duplicated(matchup_key)]

matchups_dt <- matchups_dt[!home_team==away_team]

# Next step expand matchups by experts
# "knows_about" vars are currently  discrete (0/1) with a 50/50 random split
experts_dt <- data.table(expert_id=c(1:10), 
                         knows_about_A=c(rep(0,5),rep(1,5)) %>% sample(10),
                         knows_about_B=c(rep(0,5),rep(1,5)) %>% sample(10),
                         knows_about_C=c(rep(0,5),rep(1,5)) %>% sample(10))


matchups_dt <- matchups_dt[rep(1:.N, each=nrow(experts_dt))]
matchups_dt[,expert_id:=rep(experts_dt$expert_id, uniqueN(matchup_key))]

matchups_dt[experts_dt, on="expert_id", expert_relevance:=case_when(
                                          i.knows_about_A==1 & dimension=="A" ~ 1,
                                          i.knows_about_B==1 & dimension=="B" ~ 1,
                                          i.knows_about_C==1 & dimension=="C" ~ 1,
                                          TRUE ~ 0
                                          )]

matchup_score_margin <- matchups_dt[!duplicated(matchup_key),
                                    lapply(score_margin, 
                                            split_team_score, 
                                            split_count=5,  # split_count matches the number of relevant experts set in the experts_dt
                                            max_score=10,
                                            min_score=-10,
                                            distribution="normal") %>% unlist]

matchups_dt[expert_relevance==1, expert_score:=matchup_score_margin]
matchups_dt[,.N,.(expert_relevance, is.na(expert_score))]
matchups_dt[expert_relevance==0, expert_score:=rnorm(.N,mean=0,sd=4) %>% pmin(10) %>% pmax(-10)]

# ggplot(matchups_dt, aes(x=expert_score, fill=as.factor(expert_relevance))) + geom_density(aes(alpha=.5))
# ggplot(matchups_dt[!duplicated(matchup_key)], aes(x=score_margin)) + geom_density()


# Arbitrary bounds for survey questions [10 to 5) = Blowout, [5 to 1] = Win, (1 to 0] = Tie
matchups_dt[, matchup_result:=case_when(
                expert_score > 5 ~ "Home Blowout",  
                expert_score >= 1 & expert_score <= 5 ~ "Home Win", 
                expert_score > -1 & expert_score < 1 ~ "Tie",
                expert_score >= -5 & expert_score <= -1 ~ "Away Win",
                expert_score < -5 ~ "Away Blowout"
                )]


# Write a selector . . . 
# Start with primary goals . . . to sort and start elimination . . . 

matchups_dt[,`:=` (home_win=case_when(
                    expert_score > 5 ~ 2,                      # Home Blowout 
                    expert_score >= 1 & expert_score <= 5 ~ 1, # Home Win 
                    expert_score > -1 & expert_score < 1 ~ 1,  # Tie
                    TRUE ~ 0),  
                   away_win=case_when(
                     expert_score > -1 & expert_score < 1 ~ 1, 
                     expert_score >= -5 & expert_score <= -1 ~ 1,
                     expert_score < -5 ~ 2,
                     TRUE ~ 0)
                   )]

matchups_dt[,`:=` (home_team=factor(home_team, levels=teams_dt$team), 
                   away_team=factor(away_team, levels=teams_dt$team))]

experts_only_matchups <- matchups_dt[expert_relevance==1]
