# for each method defined for class jglmm, make sure it can run on all test models

test_that("augment", {
  test_method(test_fits, augment)
})

test_that("deviance", {
  test_method(test_fits, deviance)
})

test_that("extractAIC", {
  test_method(test_fits, extractAIC)
})

test_that("fitted", {
  test_method(test_fits, fitted)
})

test_that("fixef", {
  test_method(test_fits, fixef)
})

test_that("hatvalues", {
  test_method(test_fits, hatvalues)
})

test_that("logLik", {
  test_method(test_fits, logLik)
})

test_that("nobs", {
  test_method(test_fits, nobs)
})

test_that("ranef", {
  test_method(test_fits, ranef)
})

test_that("sigma", {
  test_method(test_fits, sigma)
})

test_that("tidy", {
  test_method(test_fits, tidy)
})

test_that("vcov", {
  test_method(test_fits, vcov)
})
