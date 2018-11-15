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
