context("Selectivity Vector Length")
library(WKFORBIAS)

test_that("selectivity vector is same length as ages vector", {
  expect_equal(length(genLogisticSelx(5, 1, 1:10)), 10)
})
