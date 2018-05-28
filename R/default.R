# make the default list
#' @include smoothSpline.R
#' @include linear.R
.makeDefault <- function(protected) {
  protected <- force(protected);
  res <- unlist(c(
    # the original, raw smooth spline function
    function(metric, transformation.x, transformation.y, metric.transformed, q)
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected),

    # the smooth spline with weights
    function(metric, transformation.x, transformation.y, metric.transformed, q) {
      if(is.null(metric@weights)) { return(NULL); }
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              w=metric@weights)
    },

    # the smooth spline with using all points as knots
    function(metric, transformation.x, transformation.y, metric.transformed, q)
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              df=length(metric@x), all.knots=TRUE),

    # the smooth spline with using all points as knots and weights
    function(metric, transformation.x, transformation.y, metric.transformed, q) {
      if(is.null(metric@weights)) { return(NULL); }
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              df=length(metric@x), all.knots=TRUE,
                              w=metric@weights)
    },

    # the smooth spline not necessarily using all points as knots but many degrees of freedom
    function(metric, transformation.x, transformation.y, metric.transformed, q)
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              df=length(metric@x)),

    # the smooth spline not necessarily using all points as knots but many degrees of freedom and weights
    function(metric, transformation.x, transformation.y, metric.transformed, q) {
      if(is.null(metric@weights)) { return(NULL); }
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              df=length(metric@x),
                              w=metric@weights)
    },

    # the smooth spline with using half of the points as knots
    function(metric, transformation.x, transformation.y, metric.transformed, q)
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              df=round(0.5*length(metric@x)), all.knots=TRUE),

    # the smooth spline with using half of the points as knots and weights
    function(metric, transformation.x, transformation.y, metric.transformed, q) {
      if(is.null(metric@weights)) { return(NULL); }
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              df=round(0.5*length(metric@x)), all.knots=TRUE,
                              w=metric@weights)
    },

    # the smooth spline not necessarily using half of the points as knots but many degrees of freedom
    function(metric, transformation.x, transformation.y, metric.transformed, q)
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              df=round(0.5*length(metric@x))),

    # the smooth spline not necessarily using half of the points as knots but many degrees of freedom and weights
    function(metric, transformation.x, transformation.y, metric.transformed, q) {
      if(is.null(metric@weights)) { return(NULL); }
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              df=round(0.5*length(metric@x)),
                              w=metric@weights)
    },

    # the smooth spline with using 90% of the points as knots
    function(metric, transformation.x, transformation.y, metric.transformed, q)
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              df=round(0.9*length(metric@x)), all.knots=TRUE),

    # the smooth spline with using 90% of the points as knots and weights
    function(metric, transformation.x, transformation.y, metric.transformed, q) {
      if(is.null(metric@weights)) { return(NULL); }
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              df=round(0.9*length(metric@x)), all.knots=TRUE,
                              w=metric@weights)
    },

    # the smooth spline not necessarily using 90% of the points as knots but many degrees of freedom
    function(metric, transformation.x, transformation.y, metric.transformed, q)
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              df=round(0.9*length(metric@x))),

    # the smooth spline not necessarily using 90% of the points as knots but many degrees of freedom and weights
    function(metric, transformation.x, transformation.y, metric.transformed, q) {
      if(is.null(metric@weights)) { return(NULL); }
      regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                              protected=protected,
                              df=round(0.9*length(metric@x)),
                              w=metric@weights)
    },

    # the linear trend or constant function
    function(metric, transformation.x, transformation.y, metric.transformed, q)
      regressoR.trend.linear(metric, transformation.x, transformation.y, metric.transformed,
                             protected=protected)
  ), recursive = TRUE);

  res <- force(res);
  for(i in seq_along(res)) {
    res[[i]] <- force(res[[i]]);
  }
  res <- force(res);
  return(res);
}


# get the protected splines
.protected <- .makeDefault(TRUE);


#' @title Get the Default Protected Spline Fitters
#' @description Get the default fitters for protected splines, i.e., splines
#'   which will remain constant before and after the first and last data point,
#'   respectively.
#' @return the list of default protected splines
#' @export regressoR.spline.protected
regressoR.spline.protected <- function() .protected


# the set of default spline fitters
#' @include smoothSpline.R
#' @include linear.R
.default <- unlist(c(.protected, .makeDefault(FALSE)));

#' @title Get the Default Spline Fitters
#' @description Get the default fitters for splines.
#' @return the list of default splines
#' @export regressoR.spline.default
regressoR.spline.default <- function() .default
