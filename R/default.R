# make the default list
#' @include smoothSpline.R
#' @include linear.R
#' @include loess.R
.p.00 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.trend.linear(metric, transformation.x, transformation.y, metric.transformed, protected=TRUE);
}
.p.01 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.trend.linear(metric, transformation.x, transformation.y, metric.transformed,
                         protected=TRUE, weights=weights)
};
.p.08 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE,
                          all.knots=FALSE)};
.p.09 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE,
                          all.knots=TRUE)};
.p.10 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE,
                          all.knots=TRUE, df=length(metric@x))};
.p.12 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE,
                          all.knots=TRUE, df=max(1L, round(0.5*length(metric@x))))};
.p.14 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE,
                          all.knots=TRUE, spar=1)};
.p.16 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE,
                          all.knots=TRUE, spar=0.5)};
.p.18 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE, w=metric@weights,
                          all.knots=TRUE)
};
.p.19 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE, w=metric@weights,
                          all.knots=TRUE, df=length(metric@x))
};
.p.21 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE, w=metric@weights,
                          all.knots=TRUE, df=max(1L, round(0.5*length(metric@x))))
};
.p.23 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE, w=metric@weights,
                          all.knots=TRUE, spar=1)
};
.p.25 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE, w=metric@weights,
                          all.knots=TRUE, spar=0.5)
};
.np.00 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.trend.linear(metric, transformation.x, transformation.y, metric.transformed, protected=FALSE)};
.np.01 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.trend.linear(metric, transformation.x, transformation.y, metric.transformed,
                         protected=FALSE, weights=weights)
};
.np.08 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE,
                          all.knots=FALSE)};
.np.09 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE,
                          all.knots=TRUE)};
.np.10 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE,
                          all.knots=TRUE, df=length(metric@x))};
.np.12 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE,
                          all.knots=TRUE, df=max(1L, round(0.5*length(metric@x))));}
.np.14 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE,
                          all.knots=TRUE, spar=1)};
.np.16 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE,
                          all.knots=TRUE, spar=0.5)};
.np.18 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE, w=metric@weights,
                          all.knots=TRUE)
};
.np.19 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE, w=metric@weights,
                          all.knots=TRUE, df=length(metric@x))
};
.np.21 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE, w=metric@weights,
                          all.knots=TRUE, df=max(1L, round(0.5*length(metric@x))))
};
.np.23 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE, w=metric@weights,
                          all.knots=TRUE, spar=1)
};
.np.25 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE, w=metric@weights,
                          all.knots=TRUE, spar=0.5)
};


# get the protected splines
.protected <- c(.p.00,
                .p.01,
                .p.08,
                .p.09,
                .p.10,
                .p.12,
                .p.14,
                .p.16,
                .p.18,
                .p.19,
                .p.21,
                .p.23,
                .p.25);


#' @title Get the Default Protected Spline Fitters
#' @description Get the default fitters for protected splines, i.e., splines
#'   which will remain constant before and after the first and last data point,
#'   respectively.
#' @return the list of default protected splines
#' @export regressoR.spline.protected
regressoR.spline.protected <- function() .protected


# the set of default spline fitters
.default <- unlist(c(.protected,
                     .np.00,
                     .np.01,
                     .np.08,
                     .np.09,
                     .np.10,
                     .np.12,
                     .np.14,
                     .np.16,
                     .np.18,
                     .np.19,
                     .np.21,
                     .np.23,
                     .np.25));

#' @title Get the Default Spline Fitters
#' @description Get the default fitters for splines.
#' @return the list of default splines
#' @export regressoR.spline.default
regressoR.spline.default <- function() .default
