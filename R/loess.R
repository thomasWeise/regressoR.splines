.loess.ctl <- loess.control(surface = "direct",
                            statistics = "none");

# The internal loess fitter function
#' @importFrom stats predict loess
#' @importFrom utilizeR ignoreErrors
.loess <- function(xx, yy, ...) {
  result <- NULL;
  ignoreErrors(result <- loess(yy ~ xx, control=.loess.ctl, ...));
  result <- force(result);
  if(is.null(result)) { return(NULL); }
  f <- function(x) suppressWarnings(predict(result, x));
  f <- force(f);
  return(list(f=f, size=as.integer(ceiling(abs(result$enp))), name="loess"));
}

#' @title Apply Local Polynomial Regression Fitting a given Dataset
#' @description Local Polynomial Regression Fitting is applied to the given dataset.
#' @param metric an instance of \code{RegressionQualityMetric}
#' @param transformation.x the transformation along the \code{x}-axis, or
#'   \code{NULL} if none was applied to the data
#' @param transformation.y the transformation along the \code{y}-axis, or
#'   \code{NULL} if none was applied to the data
#' @param metric.transformed the transformed metric for the first fitting step
#' @param protected should the function be limited to the range of values
#'   actually ocurring in the data?
#' @param ... parameters to be passed to \code{\link[stats]{loess}}
#' @return On success, an instance of \code{\link{FittedSplineModel}},
#'   \code{NULL} on failure.
#' @export regressoR.loess
#' @include fitSpline.R
regressoR.loess <- function(metric,
                            transformation.x=NULL, transformation.y=NULL,
                            metric.transformed=NULL,
                            protected=TRUE,
                            ...) {
  .fitSpline(metric, .loess, transformation.x,
             transformation.y, metric.transformed,
             protected=protected,
             ...);
}
