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

# checkOMdims
test_that("check OM dimensions correct", {
  expect_equal(all(unlist(checkOMdims(
    years=2001:2010,
    nAge=5,
    nInd=2,
    nyear1list=list(values=rep(1,5)),
    Rlist=list(values=rep(1,10)),
    Mlist=list(values=matrix(1,10,5)),
    Flist=list(values=matrix(1,10,5)),
    Wlist=list(values=matrix(1,10,5)),
    indexlist=list(index01 = list(values=matrix(1,10,5)),
                   index02 = list(values=matrix(1,10,5))),
    catcherrorlist=list(values=matrix(1,10,5)),
    indexerrorlist=list(index01 = list(values=matrix(1,10,5)),
                        index02 = list(values=matrix(1,10,5))),
    processerrorlist=list(values=matrix(1,10,5)))) == TRUE), TRUE)
})

test_that("check OM dimensions bad", {
  expect_equal(all(unlist(checkOMdims(
    years=2001:2010,
    nAge=5,
    nInd=2,
    nyear1list=list(values=rep(1,5)),
    Rlist=list(values=rep(1,10)),
    Mlist=list(values=matrix(1,10,5)),
    Flist=list(values=matrix(1,10,5)),
    Wlist=list(values=matrix(1,10,15)),  # here is the bad one
    indexlist=list(index01 = list(values=matrix(1,10,5)),
                   index02 = list(values=matrix(1,10,5))),
    catcherrorlist=list(values=matrix(1,10,5)),
    indexerrorlist=list(index01 = list(values=matrix(1,10,5)),
                        index02 = list(values=matrix(1,10,5))),
    processerrorlist=list(values=matrix(1,10,5)))) == TRUE), FALSE)
})

# readASAP
test_that("check readASAP file.exists", {
  expect_equal(readASAP(NULL, NULL), "Missing ASAP file")
})
