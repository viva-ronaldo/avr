get_loser <- function(votes) {
  # Find if lowest on FPs is untied
  # If tied, move everyone NOT tied up a preference IF POSSIBLE
  # If no more preferences to go, return the tie
  fps <- get_first_preferences(votes)
  losers <- get_antimode(fps)
  # One loser, return it and move on
  if (length(losers) == 1) {
    return(losers)
  }
  # Tied losers. Remove all non-loser votes and run nested IRV
  new_votes <- drop_non_losers(votes, losers)

  if (all(sapply(votes, length) == sapply(new_votes, length))) {
    # Unavoidable ties, return the tying losers
    return(losers)
  }
  # Else recurse the reduced voter data until convergence
  new_votes <- drop_empty_votes(new_votes)
  get_loser(new_votes)
}

drop_non_losers <- function(votes, losers) {
  lapply(votes, function(vote) vote[vote %in% losers])
}
