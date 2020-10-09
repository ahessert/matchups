library("BradleyTerry2")

monte_carlo_random_bradley_terry <- function(sample_size, dt, iterations) {
  replicate(n=iterations, 
            random_sample_bradley_terry(dt, sample_size),
            simplify = FALSE) %>% rbindlist
}

random_sample_bradley_terry <- function(dt, sample_size) {
  matchup_sample <- dt[sample(.N, sample_size),
                       lapply(.SD, sum), 
                       by=.(home_team, away_team),
                       .SDcols=c("home_win", "away_win")]
  model <- BTm(cbind(home_win, away_win), 
               home_team, away_team, 
               data=matchup_sample)
  model_stats <- get_sample_stats(model,sample_method="random",sample_size)
  return(model_stats)
}

get_sample_stats <- function(model, sample_method, sample_size) {
  #Currently using team count of 50
  ab_vec <- BTabilities(model)
  top_five <- ab_vec[1:50] %>% order(decreasing = T) %>% .[1:5]
  top_five_se <- ab_vec[top_five+50] %>% (function(x){sum(x)/sum(x>0)})
  mean_se <- ab_vec[51:100] %>% sum %>% divide_by(49)
  
  list(sample_size=sample_size, 
       sample_method=sample_method, 
       pct_top5_match=sum(top_five==c(1:5))/5, 
       winner_match=top_five[1]==1,
       top_five_se=top_five_se,
       mean_se=mean_se)
}

