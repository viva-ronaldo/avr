context("Instant Runoff Voting")

data <- list(
  dex = c("Ice Skating", "Unihoc", "Food"),
  dean = c("Ice Skating", "Unihoc", "Food"),
  paul = c("Whiskey Tasting", "Established"),
  james = c("Ice Skating", "Unihoc", "Food")

)

test_that("get_first_preferences returns as expected", {
  fps <- get_first_preferences(data)
  expected_fps <- c(dex = "Ice Skating",
                    dean = "Ice Skating",
                    paul = "Whiskey Tasting",
                    james = "Ice Skating")
  expect_equal(fps, expected_fps)
})

# test_that("First-preference can win instantly", {
#   irv_soln <- irv(data)
#   expect_equal(irv_soln$winner, "Ice Skating")
# })
