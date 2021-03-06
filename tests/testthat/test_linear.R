library("regressoR.splines")
context("linear")

test_that("Test linear noiseless", {
  f <- function(x) 3 - 2*x + 0.3*x*x

  x <- runif(1000, min=0, max=6);
  #y <- rnorm(n=length(x), mean=f(x), s=0.3);
  y <- f(x);

  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);

  sfr <- regressoR.trend.linear(metric, protected=TRUE);
  expect_is(sfr, "FittedSplineModel");
  expect_identical(sfr@size, 4L);
  expect_true(all(is.finite((sfr@f(x)))));

  sfr <- regressoR.trend.linear(metric, protected=FALSE);
  expect_is(sfr, "FittedSplineModel");
  expect_identical(sfr@size, 2L);
  expect_true(all(is.finite((sfr@f(x)))));
})

test_that("Test linear noisy", {
  f <- function(x) 3 - 2*x + 0.3*x*x

  x <- runif(1000, min=0, max=6);
  y <- rnorm(n=length(x), mean=f(x), s=0.3);

  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);

  sfr <- regressoR.trend.linear(metric, protected=TRUE);
  expect_is(sfr, "FittedSplineModel");
  expect_identical(sfr@size, 4L);
  expect_true(all(is.finite((sfr@f(x)))));

  sfr <- regressoR.trend.linear(metric, protected=FALSE);
  expect_is(sfr, "FittedSplineModel");
  expect_identical(sfr@size, 2L);
  expect_true(all(is.finite((sfr@f(x)))));
})
