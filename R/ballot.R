#' Interface for running irv on ballot cards.
#'
#' @param ... Each entry in the ballot. Elements without votes cast can be
#'   noted with 0, -1, NA, or any number below 1.
#' @param map optional character mapping giving names of the voting options
#' @return ballot object
#' @export
ballot <- function(..., map = NULL) {
  votes <- c(...)
  votes[votes < 1] <- NA
  nas <- which(is.na(votes))
  ord <- seq(votes)[order(votes)]
  answer <- setdiff(ord, nas)

  if (!is.null(map)) {
    answer <- map[answer]
  }

  answer
}
