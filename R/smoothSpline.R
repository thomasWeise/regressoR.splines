# The internal spline fitter function
#' @importFrom stats smooth.spline predict
#' @importFrom utilizeR ignoreErrors
.smooth.splineFitter <- function(xx, yy, ...) {
  result <- NULL;

  ignoreErrors(result <- smooth.spline(x=xx, y=yy, keep.data=FALSE, ...));
  if(is.null(result)) {
    for(spar in c(0.75, (1-((0:10)/10)))) {
      ignoreErrors(result <- smooth.spline(x=xx, y=yy, keep.data=FALSE, spar=spar, ...));
      if(!is.null(result)) {
        break;
      }
    }
  }

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
#' @param protected should the function be limited to the range of values
#'   actually ocurring in the data?
#' @param ... parameters to be passed to \code{\link[stats]{smooth.spline}}
#' @return On success, an instance of \code{\link{FittedSplineModel}},
#'   \code{NULL} on failure.
#' @export regressoR.spline.smooth
#' @include fitSpline.R
regressoR.spline.smooth <- function(metric,
                                    transformation.x=NULL, transformation.y=NULL,
                                    metric.transformed=NULL,
                                    protected=TRUE,
                                    ...) {
  .fitSpline(metric, .smooth.splineFitter, transformation.x,
             transformation.y, metric.transformed,
             protected=protected,
             ...);
}
