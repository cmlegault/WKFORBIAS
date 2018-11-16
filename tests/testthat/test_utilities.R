context("Utilities")

test_that("Lognormal Error obs NA", {
  expect_equal(is.na(addLognormalError(NA, 0.3)), TRUE)
})

test_that("Lognormal Error sigma zero", {
  expect_equal(addLognormalError(5, 0), 5)
})

test_that("Lognormal Error input randomval", {
  expect_equal(addLognormalError(5, 1, randomval=log(0.5)), 2.5)
})

test_that("Lognormal Error bias adjustment", {
  expect_equal(addLognormalError(1, 1, biasadjustflag = TRUE, randomval = 0), exp(-0.5))
})

# calcAggregateBiomass
test_that("calc Agg B NAA NA", {
  expect_equal(is.na(calcAggregateBiomass(NA, 0.3)), TRUE)
})

test_that("calc Agg B summation", {
  expect_equal(calcAggregateBiomass(rep(100, 3), rep(0.2, 3)), 60)
})

test_that("calc Agg B indextime", {
  expect_equal(calcAggregateBiomass(rep(100, 3), rep(1, 3), 0, 0, 0.5), 300)
})
