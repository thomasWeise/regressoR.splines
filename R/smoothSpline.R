.check.spline <- function(result) {
  if(is.null(result) ||
     (!(is.list(result)) ||
      (class(result) != "smooth.spline"))) {
    return(FALSE);
  }
  return(TRUE);
}

# The internal spline fitter function
#' @importFrom stats smooth.spline predict
#' @importFrom utilizeR ignoreErrors
.smooth.splineFitter <- function(xx, yy, ...) {
  if(length(xx) < 4) { return(NULL); }
  result <- NULL;
  ignoreErrors(result <- smooth.spline(x=xx, y=yy, keep.data=FALSE, ...));
  if(!.check.spline(result)) {
    lst <- list(...);
    if(isTRUE(lst$all.knots)) {
      lst$all.knots <- FALSE;
      lst$x <- xx;
      lst$y <- yy;
      lst$keep.data <- FALSE;
      ignoreErrors(result <- do.call(smooth.spline, lst));
    }
  }
  result <- force(result);
  if(!.check.spline(result)) { return(NULL); }
  f <- function(x) predict(object=result, x=x)$y;
  r <- range(xx);
  return(tryCatch({
    # check if the spline will fail for strange reasons when fed reasonable
    # coordinates
    f(c(r[1L], r[2L], r[1L]+r[2L], r[1L]-r[2L], r[2L]-r[1L],
        1.1*r[1L], 0.9*r[1L], 1.1*r[2L], 0.9*r[2L],
        -r[1L], -r[2L]));

    # but the standard spline is not save though
    f <- function(x) {
      tryCatch(
        # sometimes it fails for no good reason
        # if the "predict" works, we are good
        predict(object=result, x=x)$y
      , error=function(e) {
          # ok it failed
          # in this case, we first sort the x coordinates
          o <- order(x);
          # then compute the y values iteratively
          y <- vapply(X=x[o], FUN=function(xx) {
            # at each computation, we catch potential errors
            # and turn them into nans
            tryCatch(predict(object=result, x=xx)$y[1L],
                     error=function(e) NaN,
                     warning=function(e) NaN)
          }, FUN.VALUE=NaN);
          # so there probably were some nans
          l <- length(y);
          for(i in seq_along(y)) {
            # so we iterate over the y coordinates
            if(is.nan(y[i])) {
              # found an nan
              f.up <- NaN;
              f.lo <- NaN;
              # we try to find non-nan values on the left and right
              if(i > 1L) {
                # first to the left
                for(j in (i-1L):1L) {
                  f.lo <- y[j];
                  if(is.finite(f.lo)) { break; }
                }
              }
              if(i < l) {
                # then to the right
                for(j in (i+1L):l) {
                  f.up <- y[j];
                  if(is.finite(f.up)) { break; }
                }
              }
              if(is.finite(f.lo)) {
                if(is.finite(f.up)) {
                  # found two, take the mean
                  y[i] <- 0.5*(f.lo + f.up);
                } else {
                  # found one, take it
                  y[i] <- f.lo;
                }
              } else {
                if(is.finite(f.up)) {
                  # found one, take it
                  y[i] <- f.up;
                }
              }
              if(!(is.finite(y[i]))) {
                # ok, found none, we are doomed, return 0
                y[i] <- 0;
              }
            }
          }
          # turn the order back into the original order, return y vactor
          return(y[order(o)]);
      })};
    f <- force(f);
    list(f=f, size=as.integer(ceiling(result$df)), name="smooth.spline");
  }, error=function(e) NULL, warning=function(e) NULL));
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
