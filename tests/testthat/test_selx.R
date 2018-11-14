context("Selectivity Functions")

test_that("selectivity vector is same length as ages vector", {
  expect_equal(length(genLogisticSelx(5, 1, 1:10)), 10)
})

test_that("Logistic A50 correct", {
  expect_equal(genLogisticSelx(5, 1, 5), 0.5)
})

test_that("Logistic asymptote correct", {
  expect_equal(genLogisticSelx(5, 1, 20), 1.0, tolerance = 0.001)
})
