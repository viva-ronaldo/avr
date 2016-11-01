context("Single Transferable Vote")

votes <- list(
  dex = c("Ice Skating", "Unihoc", "Food"),
  dean = c("Ice Skating", "Unihoc", "Food"),
  paul = c("Whiskey Tasting", "Established"),
  james = c("Ice Skating", "Unihoc", "Food"),
  ken = c("Theatre", "Other Theatre"),
  dean = c("Whiskey Tasting")
)

test_that("STV works for small data", {
  stv_soln <- stv(votes, 2)
  expect_equal(stv_soln$winners, c("Ice Skating", "Whiskey Tasting"))
  stv_soln <- stv(votes, 3)
  expect_equal(stv_soln$winners, c("Ice Skating", "Whiskey Tasting", "Unihoc"))
})
