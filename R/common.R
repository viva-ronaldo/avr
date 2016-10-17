get_all_entries <- function(votes) {
  unique(unlist(votes))
}

get_first_preferences <- function(votes) {
  unlist(sapply(votes, function(vote) vote[1]))
}

drop_not_included_in_fps <- function(remaining, fps) {
  remaining[remaining %in% fps]
}

update_prefs <- function(votes, remaining) {
  lapply(votes, function(vote) vote[vote %in% remaining])
}

drop_empty_votes <- function(votes) {
  votes[sapply(votes, function(vote) length(vote) > 0)]
}

get_lowest_voted <- function(fps) {
  tab <- table(fps)
  names(tab)[tab == min(tab)]
}
