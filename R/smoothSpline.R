# The internal spline fitter function
#' @importFrom stats smooth.spline predict
.smooth.splineFitter <- function(xx, yy) {
  result <- smooth.spline(x=xx, y=yy, keep.data=FALSE);
  result <- force(result);
  f <- function(x) predict(object=result, x=x)$y;
  f <- force(f);
  return(list(f=f, size=as.integer(ceiling(result$df)), name="smooth.spline"));
}

#' @title Fit a Smoothed Spline to a given Dataset
#' @description A smooth spline is fitted to the given dataset.
#' @param metric an instance of \code{RegressionQualityMetric}
#' @param transformation.x the transformation along the \code{x}-axis, or
#'   \code{NULL} if none was applied to the data
#' @param transformation.y the transformation along the \code{y}-axis, or
#'   \code{NULL} if none was applied to the data
#' @param metric.transformed the transformed metric for the first fitting step
#' @param forceEnd should the function be extrapolated beyond the right-most
#'   point on the \code{x}-axis (\code{FALSE}) or simply stick at the last
#'   \code{y} value (\code{TRUE})?
#' @param forceStart should the function be extrapolated beyond the left-most
#'   point on the \code{x}-axis (\code{FALSE}) or simply stick at the first
#'   \code{y} value (\code{TRUE})?
#' @return On success, an instance of \code{\link{FittedSplineModel}},
#'   \code{NULL} on failure.
#' @export regressoR.spline.smooth
#' @include fitSpline.R
regressoR.spline.smooth <- function(metric,
                                    transformation.x=NULL, transformation.y=NULL,
                                    metric.transformed=NULL,
                                    forceEnd=TRUE, forceStart=TRUE) {
  .fitSpline(metric, .smooth.splineFitter, transformation.x,
             transformation.y, metric.transformed,
             forceEnd=forceEnd, forceStart=forceStart);
}