library("regressoR.splines")
context("default 2")


.tester <- function(learners) {

  xx <- c(1L,5L,6L,7L,9L,11L,12L,14L,15L,16L,17L,19L,22L,23L,24L,26L,31L,34L,36L,37L,57L,58L,69L,74L,77L,79L,110L,114L,124L,126L,140L,141L,173L,209L,222L,249L,329L,350L,873L,2742L,12733L,13053L,20000000L);
  yy <- c(59L/430L,57L/430L,56L/430L,55L/430L,54L/430L,50L/430L,49L/430L,47L/430L,46L/430L,44L/430L,42L/430L,41L/430L,37L/430L,35L/430L,34L/430L,33L/430L,32L/430L,30L/430L,28L/430L,26L/430L,24L/430L,23L/430L,21L/430L,19L/430L,18L/430L,17L/430L,16L/430L,15L/430L,14L/430L,13L/430L,12L/430L,11L/430L,10L/430L,9L/430L,8L/430L,7L/430L,6L/430L,5L/430L,4L/430L,3L/430L,2L/430L,1L/430L,1L/430L);

  metric1 <- regressoR.quality::RegressionQualityMetric.default(xx, yy);
  trafos <- dataTransformeR::Transformation.applyDefault2D(x=xx, y=yy, TRUE);
  best <- NULL;

  for(learner in learners) {
    for(trafo in trafos) {
      if(identical(trafo@x@transformation@forward, identity) &&
         identical(trafo@y@transformation@forward, identity)) {
        metric2 <- metric1;
      } else {
        metric2 <- regressoR.quality::RegressionQualityMetric.default(trafo@x@data, trafo@y@data);
      }
      res <- learner(metric1, trafo@x@transformation, trafo@y@transformation, metric2);
      if(!(is.null(res))) { #expect_true(!(is.null(res)));
        expect_lte(res@quality, 2);
        expect_gte(res@quality, 0);
        if(is.null(best) || (best@quality > res@quality)) {
          best <- res;
        }
      }
    }
  }
}

test_that("Test protected", {
  .tester(regressoR.spline.protected());
})

test_that("Test default", {
  .tester(regressoR.spline.default());
})
