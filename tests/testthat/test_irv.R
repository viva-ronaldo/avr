context("Instant Runoff Voting")

data <- list(
  dex = c("Ice Skating", "Unihoc", "Food"),
  dean = c("Ice Skating", "Unihoc", "Food"),
  james = c("Ice Skating", "Unihoc", "Food")
)

# test_that("First-preference can win instantly", {
#   irv_soln <- irv(data)
#   expect_equal(irv_soln$winner, "Ice Skating")
# })
