context("Population Dynamics")

test_that("Catch Equation no mortality", {
  expect_equal(is.na(calcCatch(100, 0, 0)), TRUE)
})

test_that("Catch Equation NA mortality", {
  expect_equal(is.na(calcCatch(100, 0, NA)), TRUE)
})

test_that("Catch Equation F is zero", {
  expect_equal(calcCatch(100, 0.2, 0), 0)
})

test_that("Catch Equation PopN vector, point M and F", {
  expect_equal(calcCatch(rep(100, 5), 0.2, 0), rep(0, 5))
})

test_that("Catch Equation PopN, M, F all vectors", {
  expect_equal(calcCatch(rep(100, 5), rep(0.2, 5), rep(0, 5)), rep(0, 5))
})

test_that("Catch Equation PopN matrix, point M and F", {
  expect_equal(calcCatch(matrix(100, nrow = 10, ncol = 5), 0.2, 0), matrix(0, nrow = 10, ncol = 5))
})

test_that("Catch Equation PopN, M, and F all matrices", {
  expect_equal(calcCatch(matrix(100, nrow = 10, ncol = 5), matrix(0.2, nrow = 10, ncol = 5), matrix(0, nrow = 10, ncol = 5)), matrix(0, nrow = 10, ncol = 5))
})

# Cohort Survival
test_that("Cohort Survival no mortality", {
  expect_equal(calcSurvival(100, 0, 0), 100)
})

test_that("Cohort Survival no mortality vector, no plus group", {
  expect_equal(calcSurvival(rep(100, 5), 0, 0, plusgroupflag = FALSE), rep(100, 5))
})

test_that("Cohort Survival no mortality vector, with plus group", {
  expect_equal(calcSurvival(rep(100, 5), 0, 0, plusgroupflag = TRUE), c(rep(100, 4), 200))
})

# Stock Recruitment - Beverton Holt
test_that("Bev Holt SR SSB NA", {
  expect_equal(is.na(predBevHoltSR(0.2, 0.2, NA)), TRUE)
})

test_that("Bev Holt SR SSB zero", {
  expect_equal(predBevHoltSR(0.2, 0.2, 0), 0)
})

test_that("Bev Holt SR asymptote", {
  expect_equal(predBevHoltSR(100, 0.1, 10000), 100, tolerance = 0.001)
})

test_that("Bev Holt SR multiple SSB", {
  expect_equal(predBevHoltSR(100, 0, rep(5, 3)), rep(100, 3))
})

# Equilbrium Population
test_that("Equilibrium Pop Fval NA", {
  expect_equal(calcEquilibriumPop(100, 6, 0.2, NA), rep(NA, 6))
})

test_that("Equilibrium Pop zero mortality no plus group", {
  expect_equal(calcEquilibriumPop(100, 6, 0, 0, plusgroupflag = FALSE), rep(100, 6))
})
