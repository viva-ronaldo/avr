#' Instant runoff voting.
#'
#' @param votes A names list of order-of-preference vote vectors.
#' @return An IRV object, containing:
#'
#'    winner: the winning entry or entries in the case of a tie
#'
#'    nrounds: the number of rounds required to find a winner
#'
#'    thru_rounds: list of which entries were in contention at each round
#'
#'    eliminations: list of eliminated entries at each round
#' @export
#' @examples
#' votes <- list(
#'   dex = c("Ice Skating", "Unihoc", "Food"),
#'   dean = c("Ice Skating", "Unihoc", "Food"),
#'   paul = c("Whiskey Tasting", "Established"),
#'   james = c("Ice Skating", "Unihoc", "Food")
#' )
#'
#' irv_soln <- irv(votes)
#' irv_soln$winner
irv <- function(votes) {
  thru_rounds <- list()

  all_entries <- get_all_entries(votes)
  fps <- get_first_preferences(votes)
  remaining <- drop_not_included_in_fps(all_entries, fps)

  thru_rounds[[1]] <- all_entries
  thru_rounds[[2]] <- remaining

  while (length(remaining) > 1) {
    votes <- update_prefs(votes, remaining)
    votes <- drop_empty_votes(votes)

    fps <- get_first_preferences(votes)
    least_common <- get_lowest_voted(fps)

    if (is_tie(remaining, least_common)) break

    remaining <- drop_least_common(remaining, least_common)

    thru_rounds[[length(thru_rounds) + 1]] <- remaining
  }

  structure(
    list(winner = remaining,
         thru_rounds = thru_rounds,
         nrounds = length(thru_rounds) - 2,
         eliminations = get_eliminations(thru_rounds)),
    class = "IRV"
  )
}

get_eliminations <- function(thru_rounds) {
  elims <- list()
  for (i in seq(length(thru_rounds) - 1)) {
    elims[[i]] <- setdiff(thru_rounds[[i]], thru_rounds[[i+1]])
  }
  elims
}

#' @export
print.IRV <- function(irv) {
  winners <- irv$winner
  if (length(winners) > 1) {
    winners <- paste(winners, collapse = ", ")
  }

  win_str <- paste0("Winner:\t", winners,
                    "\n...in ", irv$nrounds, " rounds.")

  round <- 0
  rounds_str <- c()
  for (elimination in irv$eliminations) {
    if (length(elimination) == 0) {
      elimination <- "None"
    } else if (length(elimination) > 1) {
      elimination <- paste(elimination, collapse = ", ")
    }
    round_str <- paste0("Dropped in round ", round, ":\t", elimination, "\n")
    rounds_str <- c(rounds_str, round_str)
    round <- round + 1
  }

  message(win_str)
  message(rounds_str)
}
