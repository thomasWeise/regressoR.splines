# get the protected splines
#' @include smoothSpline.R
#' @include linear.R
.protected <- c(
  function(metric, transformation.x, transformation.y, metric.transformed, q)
    regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                            forceStart = TRUE,
                            forceEnd = TRUE),
  function(metric, transformation.x, transformation.y, metric.transformed, q)
    regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                            forceStart = TRUE,
                            forceEnd = TRUE, df=length(metric@x)),
  function(metric, transformation.x, transformation.y, metric.transformed, q)
    regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                            forceStart = TRUE,
                            forceEnd = TRUE, df=length(metric@x), all.knots=TRUE),
  function(metric, transformation.x, transformation.y, metric.transformed, q)
    regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                            forceStart = TRUE,
                            forceEnd = TRUE, df=round(0.5*length(metric@x))),
  function(metric, transformation.x, transformation.y, metric.transformed, q)
    regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                            forceStart = TRUE,
                            forceEnd = TRUE, df=round(0.5*length(metric@x)),
                            all.knots = TRUE),
  function(metric, transformation.x, transformation.y, metric.transformed, q)
    regressoR.trend.linear(metric, transformation.x, transformation.y, metric.transformed,
                            forceStart = TRUE,
                            forceEnd = TRUE)
);
.protected <- force(.protected);
for(f in .protected) { f <- force(f); }


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
.default <- unlist(c(
  .protected,
  function(metric, transformation.x, transformation.y, metric.transformed, q)
    regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                            forceStart = FALSE,
                            forceEnd = FALSE),
  function(metric, transformation.x, transformation.y, metric.transformed, q)
    regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                            forceStart = FALSE,
                            forceEnd = FALSE, df=length(metric@x)),
  function(metric, transformation.x, transformation.y, metric.transformed, q)
    regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                            forceStart = FALSE,
                            forceEnd = FALSE, df=length(metric@x), all.knots=TRUE),
  function(metric, transformation.x, transformation.y, metric.transformed, q)
    regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                            forceStart = FALSE,
                            forceEnd = FALSE, df=round(0.5*length(metric@x))),
  function(metric, transformation.x, transformation.y, metric.transformed, q)
    regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                            forceStart = FALSE,
                            forceEnd = FALSE, df=round(0.5*length(metric@x)),
                            all.knots = TRUE),
  function(metric, transformation.x, transformation.y, metric.transformed, q)
    regressoR.trend.linear(metric, transformation.x, transformation.y, metric.transformed,
                           forceStart = FALSE,
                           forceEnd = FALSE)
), recursive = TRUE);
.default <- force(.default);
for(f in .default) { f <- force(f); }

#' @title Get the Default Spline Fitters
#' @description Get the default fitters for splines.
#' @return the list of default splines
#' @export regressoR.spline.default
regressoR.spline.default <- function() .default
