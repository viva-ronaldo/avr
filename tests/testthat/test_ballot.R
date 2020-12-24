context("Ballot interface")

test_that("Ballot interface runs through IRV without a map", {
  votes <- list(
    ballot(0, 3, 1, 2, 0),
    ballot(0, 3, 1, 2, 0),
    ballot(2, 0, 0, 0, 1),
    ballot(0, 3, 1, 2, 0)
  )
  irv_soln <- irv(votes)
  expect_equal(irv_soln$winner, 3)
})

test_that("Ballot interface runs through IRV with a map", {
  map <- c("e", "f", "i", "u", "w")
  votes <- list(
    ballot(0, 3, 1, 2, 0, map = map),
    ballot(0, 3, 1, 2, 0, map = map),
    ballot(2, 0, 0, 0, 1, map = map),
    ballot(0, 3, 1, 2, 0, map = map)
  )
  irv_soln <- irv(votes)
  expect_equal(irv_soln$winner, "i")
})
