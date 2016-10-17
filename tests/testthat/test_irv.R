context("Instant Runoff Voting")

votes <- list(
  dex = c("Ice Skating", "Unihoc", "Food"),
  dean = c("Ice Skating", "Unihoc", "Food"),
  paul = c("Whiskey Tasting", "Established"),
  james = c("Ice Skating", "Unihoc", "Food")
)


# test_that("First-preference can win instantly", {
#   irv_soln <- irv(votes)
#   expect_equal(irv_soln$winner, "Ice Skating")
# })
