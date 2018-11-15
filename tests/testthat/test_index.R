context("Index Generation")

test_that("Index At Age NA mortality", {
  expect_equal(is.na(genIndexAtAge(100, 0.2, NA, 0, 1, 1)), TRUE)
})

test_that("Index At Age Full Selx q 1", {
  expect_equal(genIndexAtAge(rep(100, 5), 0.2, 0.5, 0, 1, 1), rep(100, 5))
})
