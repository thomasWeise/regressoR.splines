library("regressoR.splines")
context("smoothSpline")

test_that("Test smoothSpline noiseless", {
  f <- function(x) 3 - 2*x + 0.3*x*x

  x <- runif(1000, min=0, max=6);
  #y <- rnorm(n=length(x), mean=f(x), s=0.3);
  y <- f(x);

  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);

  sfr <- regressoR.spline.smooth(metric, protected=FALSE);
  expect_is(sfr, "FittedSplineModel");
  expect_lt(metric@quality(sfr@f), 0.1);
  expect_lt(sfr@size, length(x));
  expect_lt(sum(abs(sfr@f(x) - y)) / length(x), 0.1)

  sfr <- regressoR.spline.smooth(metric, protected=TRUE);
  expect_is(sfr, "FittedSplineModel");
  expect_lt(metric@quality(sfr@f), 0.1);
  expect_lt(sfr@size, length(x));
  expect_lt(sum(abs(sfr@f(x) - y)) / length(x), 0.1)
})

test_that("Test smoothSpline noisy", {
  f <- function(x) 3 - 2*x + 0.3*x*x

  x <- runif(1000, min=0, max=6);
  y <- rnorm(n=length(x), mean=f(x), s=0.3);

  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);

  sfr <- regressoR.spline.smooth(metric, protected=FALSE);
  expect_is(sfr, "FittedSplineModel");
  expect_lt(metric@quality(sfr@f), 1);
  expect_lt(sfr@size, length(x));
  expect_lt(sum(abs(sfr@f(x) - y)) / length(x), 1)

  sfr <- regressoR.spline.smooth(metric, protected=TRUE);
  expect_is(sfr, "FittedSplineModel");
  expect_lt(metric@quality(sfr@f), 1);
  expect_lt(sfr@size, length(x));
  expect_lt(sum(abs(sfr@f(x) - y)) / length(x), 1)
})
