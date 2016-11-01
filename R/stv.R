#' Single transferable vote
#'
#' @param votes A list of order-of-preference vote vectors, or a list of ballot
#' objects.
#' @param tiebreak String informing tiebreak behavior.
#'   \describe{
#'     \item{"all"}{All tied losers will be dropped}
#'     \item{"random"}{Tied losers will be dropped randomly}
#'     \item{"nested"}{Tied losers will be broken by running a nested STV only
#'                     considering votes for the losers.}
#'   }
#'
#' @return An STV object, containing:
#'   \describe{
#'     \item{winner:}{the winning entry or entries in the case of a tie}
#'     \item{nrounds:}{the number of rounds required to find a winner}
#'     \item{rem_rounds:}{list of which entries were in contention at each round}
#'     \item{fps_rounds:}{table of first preference votes through each round}
#'     \item{eliminations:}{list of eliminated entries at each round}
#'   }
#' @export
#' @examples
#' votes <- list(
#'   dex = c("Ice Skating", "Unihoc", "Food"),
#'   dean = c("Ice Skating", "Unihoc", "Food"),
#'   paul = c("Whiskey Tasting", "Established"),
#'   james = c("Ice Skating", "Unihoc", "Food")
#' )
#'
#' stv(votes)
#'
#' map <- c("e", "f", "i", "u", "w")
#' votes <- list(
#'   ballot(0, 3, 1, 2, 0, map = map),
#'   ballot(0, 3, 1, 2, 0, map = map),
#'   ballot(2, 0, 0, 0, 1, map = map),
#'   ballot(0, 3, 1, 2, 0, map = map)
#' )
#' stv(votes)
stv <- function(votes, nseats, tiebreak = "all") {
  winners <- c()
  nvotes <- length(votes)
  weights <- rep(1, nvotes)
  quota <- droop_quota(nvotes, nseats)

  while (length(winners) < nseats) {
    fps <- get_first_preferences(votes)
    through_this_rnd <- get_above_quota(fps, quota, weights)

    if (length(through_this_rnd) > 0) {
      # Move everyone's votes up and discount weights for each winner
      for (winner in through_this_rnd) {
        nvotes <- get_nvotes(fps, winner, weights)
        excess <- nvotes - quota
        transfer_ratio <- excess / nvotes
        weights[fps == winner] <- weights[fps == winner] * transfer_ratio
      }
      # Have to update quota for any dropped votes
      votes <- remove_prefs(votes, through_this_rnd)
      votes <- drop_empty_votes(votes)
      quota <- droop_quota(length(votes), nseats)
    } else {
      # Eliminate someone, update votes with no change to weightsc
      least_common <- get_stv_loser(fps, weights)
      votes <- remove_prefs(votes, winners_this_rnd)
      votes <- drop_empty_votes(votes)
      quota <- droop_quota(length(votes), nseats)
    }
  }

  structure(
    list(winner = winners,
         rem_rounds = rem_rounds,
         fps_rounds = fps_rounds,
         nrounds = length(rem_rounds) - 2,
         eliminations = get_eliminations(rem_rounds)),
    class = "STV"
  )
}

#' Return the names of the candidates who are over the current quota
get_above_quota <- function(fps, quota) {
  ufps <- unique(fps)
  sum_wts <- sapply(ufps, function(fp) sum(weights[fps == fp]))

  ufps[sum_wts > quota]
}

#' Get the number of votes for a given candidate
get_nvotes <- function(fps, candidate, weights) {
  sum(weights[fps == candidate])
}
