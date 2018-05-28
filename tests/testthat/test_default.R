library("regressoR.splines")
context("default")

.tester <- function(sl) {
  set.seed(124L);

  f <- function(x) 3 - 2*x + 0.3*x*x + 2*sin(7*x)
  x <- runif(1000, min=0, max=6);
  y <- f(x);
  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);

  for(s in sl) {
    sfr <- s(metric, NULL, NULL, NULL, 0);
    expect_is(sfr, "FittedSplineModel");
    expect_lte(sfr@size, length(x) + 3);
    expect_true(all(is.finite(sfr@f(x))));
    if(sfr@size > 15L) {
      expect_lt(metric@quality(sfr@f), 0.1);
      expect_lt(sum(abs(sfr@f(x) - y)) / length(x), 0.1)
    }
  }

  y <- rnorm(n=length(x), mean=f(x), s=0.3);
  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);

  for(s in sl) {
    sfr <- s(metric, NULL, NULL, NULL, 0);
    expect_lte(sfr@size, length(x)+3);
    expect_true(all(is.finite(sfr@f(x))));
    if(sfr@size > 4L) {
      expect_lt(metric@quality(sfr@f), 0.75);
      expect_lt(sum(abs(sfr@f(x) - y)) / length(x), 1)
    }
  }
}

test_that("Test protected", {
  .tester(regressoR.spline.protected());
})

test_that("Test default", {
  .tester(regressoR.spline.default());
})
