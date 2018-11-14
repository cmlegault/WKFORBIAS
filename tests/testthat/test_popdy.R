context("Population Dynamics")

test_that("No mortality", {
  expect_equal(is.na(calcCatch(100, 0, 0)), TRUE)
})

test_that("NA mortality", {
  expect_equal(is.na(calcCatch(100, 0, NA)), TRUE)
})

test_that("F is zero", {
  expect_equal(calcCatch(100, 0.2, 0), 0)
})

test_that("PopN vector, point M and F", {
  expect_equal(calcCatch(rep(100, 5), 0.2, 0), rep(0, 5))
})

test_that("PopN, M, F all vectors", {
  expect_equal(calcCatch(rep(100, 5), rep(0.2, 5), rep(0, 5)), rep(0, 5))
})

test_that("PopN matrix, point M and F", {
  expect_equal(calcCatch(matrix(100, nrow = 10, ncol = 5), 0.2, 0), matrix(0, nrow = 10, ncol = 5))
})

test_that("PopN, M, and F all matrices", {
  expect_equal(calcCatch(matrix(100, nrow = 10, ncol = 5), matrix(0.2, nrow = 10, ncol = 5), matrix(0, nrow = 10, ncol = 5)), matrix(0, nrow = 10, ncol = 5))
})

