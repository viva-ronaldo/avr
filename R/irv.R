#' Instant runoff voting.
#'
#' @param votes A names list of order-of-preference vote vectors.
#' @return An IRV object, containing:
#'
#'    winner: the winning entry
#'
#'    ???: more to come!
#' @export
irv <- function(votes) {
  all_entries <- get_all_entries(votes)
  fps <- get_first_preferences(votes)
  remaining <- drop_not_included_in_fps(all_entries, fps)

  while(length(remaining) > 1) {
    votes <- update_prefs(votes, remaining)
    votes <- drop_empty_votes(votes)

    fps <- get_first_preferences(votes)
    least_common <- get_lowest_voted(fps)

    remaining <- drop_least_common(remaining, least_common)
  }

  list(winner = remaining)
}
