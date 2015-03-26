## Copyright (C) 2011-2015 Gray Calhoun; MIT license

test_that("dmw_lambda works as expected", {
  expect_that(dmw_lambda(Inf, "recursive"), equals(list(fh = 1, hh = 2)))
  expect_that(dmw_lambda(Inf, "rolling"),   equals(list(fh = 1, hh = 1)))
  expect_that(dmw_lambda(Inf, "fixed"),     equals(list(fh = 0, hh = Inf)))
  expect_that(dmw_lambda(0, "recursive"), equals(list(fh = 0, hh = 0)))
  expect_that(dmw_lambda(0, "rolling"),   equals(list(fh = 0, hh = 0)))
  expect_that(dmw_lambda(0, "fixed"),     equals(list(fh = 0, hh = 0)))
})
