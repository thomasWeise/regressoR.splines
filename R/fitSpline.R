# Fit a Spline
#
# @param metric an instance of \code{RegressionQualityMetric}
# @param splineFitter a function \code{f(x, y)} returning a list \code(list(f=splineFunc,
#   size=numberOfPoints, name=name))
# @param par the initial starting point
# @param transformation.x the transformation along the \code{x}-axis, or
#   \code{NULL} if none was applied to the data
# @param transformation.y the transformation along the \code{y}-axis, or
#   \code{NULL} if none was applied to the data
# @param metric.transformed the transformed metric for the first fitting step
#
# @return On success, an instance of \code{\link{FittedSplineModel}}.
#   \code{NULL} on failure.
#' @importFrom learnerSelectoR learning.checkQuality
#' @importFrom utilizeR ignoreErrors
#' @include FittedSplineModel.R
.fitSpline <- function(metric,
                       splineFitter,
                       transformation.x=NULL, transformation.y=NULL,
                       metric.transformed=NULL,
                       protected=TRUE,
                       ...) {

  if(is.null(splineFitter) || (!(is.function(splineFitter)))) {
    stop("splineFitter must be a proper function.");
  }

  # First we check the transformations whether they are NULL or identity
  # transformations.
  f.x.i <- is.null(transformation.x);
  if(!f.x.i) {
    f.x <- transformation.x@forward;
    f.x.i <- identical(f.x, identity);
  }

  f.y.i <- is.null(transformation.y);
  if(!f.y.i) {
    f.y <- transformation.y@backward;
    f.y.i <- identical(f.y, identity);
  }

  if(protected) {
    # get the original data
    xx <- metric@x;
    yy <- metric@y;
    or <- order(xx);

    # setup boundary values
    i <- or[1L];
    x.min  <- xx[i]; x.min  <- force(x.min);
    y.xmin <- yy[i]; y.xmin <- force(y.xmin);
    i <- or[length(xx)];
    x.max  <- xx[i]; x.max  <- force(x.max);
    y.xmax <- yy[i]; y.xmax <- force(y.xmax);
  }

  # initialize result
  result        <- NULL;
  trafo.complex <- 0L;

  # get result
  if(f.x.i && f.y.i) {
    # Both transformations are NULL or identity transformations
    if(is.null(metric.transformed) ||
       identical(metric.transformed, metric)) {
      # OK, we fit on the original, raw data. The transformations are identity
      # or NULL and the transformed metric is NULL or identical to the actual
      # metric.
      # Then, we fit the spline directly on the original data and are good
      if(!protected) {
        xx <- metric@x;
        yy <- metric@y;
        or <- order(xx);
      }
      ignoreErrors(result <- (splineFitter(x=xx[or], y=yy[or], ...)));
      if(is.null(result)) {
        return(NULL);
      }
    } else {
      stop("Transformed metric must be identical to actual metric or NULL if transformations are both NULL or identity.");
    }
  } else {
    if(is.null(metric.transformed)) {
      stop("Transformed metric canot be NULL if at least one transformation is not NULL or identity.");
    }
  }
  xx <- NULL; yy <- NULL; or <- NULL;

  if(is.null(result)) {
    # OK, we need to deal with the transformed data

    xx <- metric.transformed@x;
    yy <- metric.transformed@y;
    or <- order(xx);

    # The first fitting step takes place on the transformed data.
    ignoreErrors(result <- (splineFitter(x=xx[or], y=yy[or], ...)));
    if(is.null(result)) {
      return(NULL);
    }

    xx <- NULL; yy <- NULL; or <- NULL;
  }

  # take the function
  f <- result$f;
  nameAdd <- "";

  # get function for raw data
  if(f.x.i) {
    if(f.y.i) {
      # no transformation
      f.n <- f;
    } else {
      # x is identity, y is not
      f.n <- function(x) f.y(f(x));
      trafo.complex <- transformation.y@complexity;
      nameAdd <- " with transformed y";
    }
  } else {
    # x is not identity
    if(f.y.i) {
      # y is identity, x not
      f.n <- function(x) f(f.x(x));
      trafo.complex <- transformation.x@complexity;
      nameAdd <- " with transformed x";
    } else {
      # neither is
      f.n <- function(x) f.y(f(f.x(x)));
      trafo.complex <- transformation.x@complexity +
                       transformation.y@complexity;
      nameAdd <- " with transformed xy";
    }
  }

  # build the resulting spline function
  namePrefix <- "";
  limitAdd <- 0L;

  if(protected) {
    # hold both the end and the start

    namePrefix <- "protected ";
    limitAdd   <- 2L;

    if(y.xmin < y.xmax) {
      ymin <- y.xmin;
      ymax <- y.xmax;
    } else {
      ymin <- y.xmax;
      ymax <- y.xmin;
    }
    ymin <- force(ymin);
    ymax <- force(ymax);

    f <- function(x) {
      y <- vector(mode="double", length=length(x));
      a <- x <= x.min;   # get positions of values which are too small
      y[a] <- y.xmin;    # set these values
      b <- x >= x.max;   # get positions of values too big
      y[b] <- y.xmax;    # set these values
      a <- !(a | b);     # get positions of remaining values
      if(any(a)) {
        y[a] <- f.n(x[a]); # compute these values
      }
      y[y > ymax] <- ymax; # fix maximum
      y[y < ymin] <- ymin; # fix minimum
      y                  # return result
    }

  } else {
    # don't need to hold any start or end values
    f <- f.n;
  }

  # compute the quality of the spline
  quality <- metric@quality(f);
  if(learning.checkQuality(quality)) {
    return(FittedSplineModel.new(f, quality,
         # size is spline size, plus 2 for the limit points, plus the
         # transformation
           result$size + limitAdd + trafo.complex,
           paste(namePrefix, result$name, nameAdd,
                 sep="", collapse="")));
  }

  # ok, the spline is somehow invalid
  return(NULL);
}
