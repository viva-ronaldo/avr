context("Instant Runoff Voting")

votes <- list(
  dex = c("Ice Skating", "Unihoc", "Food"),
  dean = c("Ice Skating", "Unihoc", "Food"),
  paul = c("Whiskey Tasting", "Established"),
  james = c("Ice Skating", "Unihoc", "Food")
)

test_that("First-preference can win instantly", {
  irv_soln <- irv(votes)
  expect_equal(irv_soln$winner, "Ice Skating")
})

test_that("First-round ties handled appropriately", {
  votes$ken <- c("Theatre", "Other Theatre")
  votes$dean <- c("Whiskey Tasting")
  irv_soln <- irv(votes)
  expect_equal(irv_soln$winner, c("Ice Skating", "Whiskey Tasting"))
})


votes <- list(
  n1 = c("a", "b", "c", "d"),
  n2 = c("a", "b", "c", "d"),
  n3 = c("a", "b", "c", "d"),
  n4 = c("b", "a", "c", "d"),
  n5 = c("b", "a", "c", "d"),
  n6 = c("c", "b", "c", "d"),
  n7 = c("c", "b", "c", "d"),
  n8 = c("d", "c", "c", "d")
)

test_that("Through-round reporting is accurate", {
  irv_soln <- irv(votes)
  expected_rounds <- list(c("a", "b", "c", "d"),
                          c("a", "b", "c", "d"),
                          c("a", "b", "c"),
                          c("a", "c"),
                          c("a"))
  expect_equal(irv_soln$rem_rounds, expected_rounds)
})

test_that("Eliminations reporting is accurate", {
  irv_soln <- irv(votes)
  expected_elims <- list(character(0),
                         c("d"),
                         c("b"),
                         c("c"))
  expect_equal(irv_soln$eliminations, expected_elims)
})
