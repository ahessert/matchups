split_team_score <- function(team_score, 
                             split_count, 
                             max_score=10, 
                             min_score=0, 
                             distribution="uniform") {
  
  agg_team_score <- team_score * split_count
  split_scores <- c()
  
  for (atrribute_idx in seq(1,split_count,1)) {
    remaining_splits <- split_count-atrribute_idx
    if (atrribute_idx == split_count) {
      split_score <- agg_team_score
    } else {
      split_min <- max(agg_team_score-(max_score*remaining_splits), min_score)
      split_max <- min(agg_team_score-(min_score*remaining_splits), max_score)
      split_score <- get_split_score(split_min, split_max, team_score, distribution)
    }
    split_scores <- c(split_scores, split_score)
    agg_team_score <- agg_team_score - split_score
  }
  
  check_splits(team_score, split_count, min_score, max_score, split_scores)
  return(sample(split_scores))
}


get_split_score <- function(split_min, split_max, team_score, distribution="uniform") {
  if (distribution == "uniform") {
    split_score <- runif(1, split_min, split_max)
  }
  if (distribution == "normal") {
    split_score <- rnorm(1, mean=team_score, sd=3) %>% pmin(split_max) %>% pmax(split_min)
  }
  return(split_score)
}


check_splits <- function(team_score, split_count, min_score, max_score, split_scores) {
  if (sum(is.na(split_scores)) > 0) {
    stop("Splits contain NULLs", 
         "\n\tSplit scores generated: ", paste(split_scores, collapse=", "))
  }
  if (length(split_scores) != split_count) {
    stop("Not enough splits\n\tAsked for: ", split_count, 
         "\n\tGot: ", length(split_scores))
  }
  if (!all.equal(mean(split_scores), team_score)) {
    stop("Split scores do not average to team score\n\tTeam score: ", 
         team_score, "\n\tsplit avg: ", mean(split_scores))
  }
  if (min(split_scores) < min_score) {
    stop("Split scores contain values below the min score\n\tScore minimum: ", min_score,
         "\n\tSplit scores: ", paste(split_scores, collapse=", "))
  }
  if (max(split_scores) > max_score) {
    stop("Split scores contain values below the min score\n\tScore maximum: ", max_score,
         "\n\tSplit scores: ", paste(split_scores, collapse=", "))
  }
}
