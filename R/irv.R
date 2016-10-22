#' Instant runoff voting.
#'
#' @param votes A list of order-of-preference vote vectors, or a list of ballot
#' objects.
#' @param tiebreak String informing tiebreak behavior.
#'   \describe{
#'     \item{"all"}{All tied losers will be dropped}
#'     \item{"random"}{Tied losers will be dropped randomly}
#'     \item{"nested"}{Tied losers will be broken by running a nested IRV only
#'                     considering votes for the losers.}
#'   }
#'
#' @return An IRV object, containing:
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
#' irv(votes)
#'
#' map <- c("e", "f", "i", "u", "w")
#' votes <- list(
#'   ballot(0, 3, 1, 2, 0, map = map),
#'   ballot(0, 3, 1, 2, 0, map = map),
#'   ballot(2, 0, 0, 0, 1, map = map),
#'   ballot(0, 3, 1, 2, 0, map = map)
#' )
#' irv(votes)
irv <- function(votes, tiebreak = "all") {
  get_loser <- get_loser_fn(tiebreak)

  all_entries <- get_all_entries(votes)
  fps <- get_first_preferences(votes)
  remaining <- drop_not_included_in_fps(all_entries, fps)

  rem_rounds <- list(all_entries, remaining)
  fps_rounds <- list(table(fps))

  while (length(remaining) > 1) {
    if (any_has_majority(fps)) {
      remaining <- get_mode(fps)
      break
    }

    votes <- update_prefs(votes, remaining)
    votes <- drop_empty_votes(votes)

    fps <- get_first_preferences(votes)
    least_common <- get_loser(votes)

    if (is_tie(remaining, least_common)) break

    remaining <- drop_least_common(remaining, least_common)

    rem_rounds[[length(rem_rounds) + 1]] <- remaining
    fps_rounds[[length(fps_rounds) + 1]] <- table(fps)
  }

  structure(
    list(winner = remaining,
         rem_rounds = rem_rounds,
         fps_rounds = fps_rounds,
         nrounds = length(rem_rounds) - 2,
         eliminations = get_eliminations(rem_rounds)),
    class = "IRV"
  )
}

get_loser_fn <- function(tiebreak) {
  if (tiebreak == "all") return(get_loser_all)
  if (tiebreak == "random") return(get_loser_random)
  if (tiebreak == "nested") return(get_loser_nested)
}

get_eliminations <- function(rem_rounds) {
  elims <- list()
  for (i in seq(length(rem_rounds) - 1)) {
    elims[[i]] <- setdiff(rem_rounds[[i]], rem_rounds[[i+1]])
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
