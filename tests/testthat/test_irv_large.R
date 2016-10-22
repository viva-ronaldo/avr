context("Reasonable-size data IRV tests")

m01 <- ballot( 0,  2,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0)
m02 <- ballot( 0,  9,  8, 10, 12,  4,  2,  3, 11,  5, 13,  6,  1,  7)
m03 <- ballot( 0,  3,  4,  5,  7,  9,  0,  1,  8,  2, 10,  0,  0,  6)
m04 <- ballot( 0,  1,  2,  5,  0,  0,  0,  0,  0,  3,  6,  0,  0,  4)
m05 <- ballot( 5,  3,  4, 12,  1, 14, 10, 13,  6,  7,  8,  2,  9, 11)
m06 <- ballot( 1, 10,  4,  0,  9,  7,  6,  8,  3,  2,  11, 5, 12,  0)
m07 <- ballot(14, 11, 12,  2, 10,  7,  9,  8,  1,  5,  4,  6, 13,  3)
m08 <- ballot(13,  4,  3,  8, 14,  1,  2, 12,  7,  6,  5,  9, 11, 10)
m09 <- ballot( 9,  7,  8,  3,  5,  4, 14,  2, 11,  1, 10, 13, 12,  6)
m10 <- ballot( 9,  6,  7,  5, 10,  0,  0,  0,  3,  4,  2,  1,  8, 11)
m11 <- ballot( 0,  2,  3,  1, 11,  0,  7,  6,  8,  9,  4, 10,  0,  5)
m12 <- ballot(15,  9,  1,  5, 14,  8,  3,  2,  5,  4,  6, 13,  7, 10)
m13 <- ballot( 1,  3,  4,  8,  2, 13,  5,  9,  6,  7, 11, 12, 10, 14)
m14 <- ballot(14,  1,  2,  7,  8,  3,  5, 10,  9, 13,  4, 11, 12,  6)
m15 <- ballot(14, 12,  1,  6, 13,  5, 10, 11,  7,  2,  8,  4,  3,  9)
m16 <- ballot( 8,  7,  6, 14,  5,  2,  1,  3,  9,  4, 13, 11, 10, 12)
m17 <- ballot( 3,  1,  2, 11, 12,  5,  4, 14,  6,  8,  7,  9, 10, 13)
m18 <- ballot(12,  5,  4,  1, 13,  6, 14, 11,  7,  2,  8, 10,  9,  3)
m19 <- ballot( 9,  6,  8,  4, 13, 12, 11,  9,  3,  2, 10,  5,  7,  1)

irv_soln <- irv(
  list(
    m01,
    m02,
    m03,
    m04,
    m05,
    m06,
    m07,
    m08,
    m09,
    m10,
    m11,
    m12,
    m13,
    m14,
    m15,
    m16,
    m17,
    m18,
    m19
  ),
  tiebreak = "nested"
)

test_that("Reasonable-size data IRV winner is correct", {
  expect_equal(irv_soln$winner, 10)
})

test_that("Reasonable-size data IRV nrounds is correct", {
  expect_equal(irv_soln$nrounds, 12)
})
