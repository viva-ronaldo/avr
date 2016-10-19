context("Common utilities")

votes <- list(
  dex = c("Ice Skating", "Unihoc", "Food"),
  dean = c("Ice Skating", "Unihoc", "Food"),
  paul = c("Whiskey Tasting", "Established"),
  james = c("Ice Skating", "Unihoc", "Food")
)

test_that("get_all_entries returns as expected", {
  entries <- get_all_entries(votes)
  expected_entries <- c("Ice Skating", "Unihoc", "Food",
                        "Whiskey Tasting", "Established")
  expect_equal(sort(entries), sort(expected_entries))
})

test_that("get_first_preferences returns as expected", {
  fps <- get_first_preferences(votes)
  expected_fps <- c(dex = "Ice Skating",
                    dean = "Ice Skating",
                    paul = "Whiskey Tasting",
                    james = "Ice Skating")
  expect_equal(fps, expected_fps)
})

test_that("drop_not_included_in_fps returns as expected", {
  remaining <- c("a", "b", "c")
  fps <- c("a", "c")
  dropped <- drop_not_included_in_fps(remaining, fps)
  expect_equal(dropped, c("a", "c"))
})

test_that("update_prefs returns as expected", {
  remaining <- c("Unihoc", "Whiskey Tasting")
  votes_update <- update_prefs(votes, remaining)

  votes_expected <- list(dex = "Unihoc",
                         dean = "Unihoc",
                         paul = "Whiskey Tasting",
                         james = "Unihoc")
  expect_equal(votes_update, votes_expected)
})

test_that("drop_empty_votes and update_prefs correctly iterate", {
  remaining <- c("Unihoc", "Food")
  votes_update <- update_prefs(votes, remaining)
  votes_update <- drop_empty_votes(votes_update)

  votes_expected <- list(dex = c("Unihoc", "Food"),
                         dean = c("Unihoc", "Food"),
                         james = c("Unihoc", "Food"))
  expect_equal(votes_update, votes_expected)
})

test_that("get_lowest_voted returns as expected", {
  votes$ken <- c("Theatre", "Other Theatre")
  fps <- get_first_preferences(votes)
  lowest <- get_lowest_voted(fps)
  expect_equal(sort(lowest), c("Theatre", "Whiskey Tasting"))
})
