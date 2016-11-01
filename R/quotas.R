droop_quota <- function(votes_cast, seats_to_fill) {
  floor(votes_cast / (seats_to_fill + 1)) + 1 - .Machine$double.eps
}
