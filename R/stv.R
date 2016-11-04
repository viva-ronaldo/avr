#' Single transferable vote
#'
#' @param votes A list of order-of-preference vote vectors, or a list of ballot
#' objects.
#' @param nseats How many seats to fill.
#'
#' @return An STV object, containing:
#'   \describe{
#'     \item{winners:}{the winning entries in order of preference}
#'     \item{More to come!}{}
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
#' stv(votes, 2)
#'
#' map <- c("e", "f", "i", "u", "w")
#' votes <- list(
#'   ballot(0, 3, 1, 2, 0, map = map),
#'   ballot(0, 3, 1, 2, 0, map = map),
#'   ballot(2, 0, 0, 0, 1, map = map),
#'   ballot(0, 3, 1, 2, 0, map = map)
#' )
#' stv(votes, 2)
stv <- function(votes, nseats) {
  winners <- c()
  nvotes <- length(votes)
  weights <- rep(1, nvotes)
  quota <- droop_quota(nvotes, nseats)


  while (length(winners) < nseats) {
    fps <- get_first_preferences(votes)
    through_this_rnd <- get_above_quota(fps, quota, weights)

    if (length(fps) == 0) {
      break
    }

    if (length(through_this_rnd) > 0) {
      # Move everyone's votes up and discount weights for each winner
      for (winner in through_this_rnd) {
        nvotes <- get_nvotes(fps, winner, weights)
        excess <- nvotes - quota
        transfer_ratio <- excess / nvotes
        weights[fps == winner] <- weights[fps == winner] * transfer_ratio
      }
      winners <- c(winners, through_this_rnd)
      # Have to update quota for any dropped votes
      votes <- remove_prefs(votes, through_this_rnd)
    } else {
      # Eliminate someone, update votes with no change to weights
      loser <- get_stv_loser(fps, weights)
      votes <- remove_prefs(votes, loser)
    }
    votes <- drop_empty_votes(votes)
    quota <- droop_quota(length(votes), nseats)
  }

  structure(
    list(winners = winners),
    class = "STV"
  )
}

#' Return the names of the candidates who are over the current quota
get_above_quota <- function(fps, quota, weights) {
  ufps <- unique(fps)
  sum_wts <- sapply(ufps, function(fp) sum(weights[fps == fp]))

  ufps[sum_wts >= quota]
}

#' Get the number of votes for a given candidate
get_nvotes <- function(fps, candidate, weights) {
  sum(weights[fps == candidate])
}

#' Return the lowest-voted candidate by FPs, resolving ties randomly
get_stv_loser <- function(fps, weights) {
  ufps <- unique(fps)
  sum_wts <- sapply(ufps, function(fp) sum(weights[fps == fp]))
  losers <- ufps[sum_wts == min(sum_wts)]
  if (length(losers) == 1) {
    return(losers)
  }
  sample(losers, 1)
}

#' @export
print.STV <- function(stv) {
  message("An avr irv object.")
  winners <- stv$winners
  message("Winners:")
  for (i in seq_along(winners)) {
    str <- paste0("Round ", i, ":\t", winners[i])
    message(str)
  }
  invisible()
}
