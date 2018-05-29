# The internal linear trend fitter function
#' @importFrom stats lm
.linear.fitter <- function(xx, yy, ...) {
  res <- lm(yy ~ xx, model=FALSE, x=FALSE, y=FALSE, ...);

  if(is.null(res)) { return(NULL); }
  if(is.null(res$coefficients) ||
    (length(res$coefficients) != 2L)) { return(NULL);}
  a <- res$coefficients[[1L]];
  if(is.na(a)) { a <- 0; }
  a <- force(a);

  b <- res$coefficients[[2L]];
  if(is.na(b)) { b <- 0; }
  b <- force(b);

  if(is.nan(a) || is.nan(b)) { return(NULL); }

  if(abs(b) < 1e-15) {
    f <- function(x) a; f <- force(f);
    return(list(f=f, size=1L, name="constant trend"));
  }

  f <- function(x) a+b*x;      f <- force(f);
  return(list(f=f, size=2L, name="linear trend"));
}

#' @title Fit a Linear Trend to a given Dataset
#' @description A linear trend is fitted to the given dataset.
#' @param metric an instance of \code{RegressionQualityMetric}
#' @param transformation.x the transformation along the \code{x}-axis, or
#'   \code{NULL} if none was applied to the data
#' @param transformation.y the transformation along the \code{y}-axis, or
#'   \code{NULL} if none was applied to the data
#' @param metric.transformed the transformed metric for the first fitting step
#' @param protected should the function be limited to the range of values
#'   actually ocurring in the data?
#' @param ... parameters to be passed to \code{\link[stats]{lm}} directly
#' @return On success, an instance of \code{\link{FittedSplineModel}},
#'   \code{NULL} on failure.
#' @export regressoR.trend.linear
#' @include fitSpline.R
regressoR.trend.linear <- function(metric,
                                   transformation.x=NULL, transformation.y=NULL,
                                   metric.transformed=NULL,
                                   protected=TRUE,
                                   ...) {
  .fitSpline(metric, .linear.fitter, transformation.x,
             transformation.y, metric.transformed,
             protected=protected);
}
