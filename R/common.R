get_first_preferences <- function(data) {
  unlist(sapply(data, function(votes) votes[1]))
}
