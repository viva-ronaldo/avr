get_all_entries <- function(votes) {
  unique(unlist(votes))
}

get_first_preferences <- function(votes) {
  unlist(sapply(votes, function(vote) vote[1]))
}

drop_not_included_in_fps <- function(remaining, fps) {
  remaining[remaining %in% fps]
}

drop_least_common <- function(remaining, lcs) {
  remaining[!remaining %in% lcs]
}

is_tie <- function(remaining, lcs) {
  if (all(remaining %in% lcs)) {
    return(TRUE)
  }
  FALSE
}

any_has_majority <- function(fps) {
  tab <- table(fps)
  threshold <- floor((length(fps) / 2) + 1)
  if (max(tab) >= threshold) {
    return(TRUE)
  }
  FALSE
}

#' From stackoverflow.com/questions/2547402
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
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
