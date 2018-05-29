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
.p.02 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=TRUE,
                  span=0.75) };
.p.26 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=TRUE, weights=weights,
                  span=0.75)
};
.p.03 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=TRUE,
                  span=0.5) };
.p.27 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=TRUE, weights=weights,
                  span=0.5)
};
.p.04 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=TRUE,
                  span=0.25) };
.p.05 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=TRUE, weights=weights,
                  span=0.25)
};
.p.06 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=TRUE,
                  span=0.05)};
.p.07 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=TRUE, weights=weights,
                  span=0.05)
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
.p.11 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE,
                          all.knots=FALSE, df=max(1L, round(0.5*length(metric@x))))};
.p.12 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE,
                          all.knots=TRUE, df=max(1L, round(0.5*length(metric@x))))};
.p.13 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE,
                          all.knots=FALSE, spar=1)};
.p.14 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE,
                          all.knots=TRUE, spar=1)};
.p.15 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE,
                          all.knots=FALSE, spar=0.5)};
.p.16 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE,
                          all.knots=TRUE, spar=0.5)};
.p.17 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE, w=metric@weights,
                          all.knots=FALSE)
};
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
.p.20 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE, w=metric@weights,
                          all.knots=FALSE, df=max(1L, round(0.5*length(metric@x))))
};
.p.21 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE, w=metric@weights,
                          all.knots=TRUE, df=max(1L, round(0.5*length(metric@x))))
};
.p.22 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE, w=metric@weights,
                          all.knots=FALSE, spar=1)
};
.p.23 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE, w=metric@weights,
                          all.knots=TRUE, spar=1)
};
.p.24 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=TRUE, w=metric@weights,
                          all.knots=FALSE, spar=0.5)
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
.np.02 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=FALSE,
                  span=0.75)};
.np.26 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=FALSE, weights=weights,
                  span=0.75)
};
.np.03 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=FALSE,
                  span=0.5)};
.np.27 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=FALSE, weights=weights,
                  span=0.5)
};
.np.04 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=FALSE,
                  span=0.25)};
.np.05 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=FALSE, weights=weights,
                  span=0.25)
};
.np.06 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=FALSE,
                  span=0.05)};
.np.07 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.loess(metric, transformation.x, transformation.y, metric.transformed,
                  protected=FALSE, weights=weights,
                  span=0.05)
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
.np.11 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE,
                          all.knots=FALSE, df=max(1L, round(0.5*length(metric@x))))};
.np.12 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE,
                          all.knots=TRUE, df=max(1L, round(0.5*length(metric@x))));}
.np.13 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE,
                          all.knots=FALSE, spar=1)};
.np.14 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE,
                          all.knots=TRUE, spar=1)};
.np.15 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE,
                          all.knots=FALSE, spar=0.5)};
.np.16 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE,
                          all.knots=TRUE, spar=0.5)};
.np.17 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE, w=metric@weights,
                          all.knots=FALSE)
};
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
.np.20 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE, w=metric@weights,
                          all.knots=FALSE, df=max(1L, round(0.5*length(metric@x))))
};
.np.21 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE, w=metric@weights,
                          all.knots=TRUE, df=max(1L, round(0.5*length(metric@x))))
};
.np.22 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE, w=metric@weights,
                          all.knots=FALSE, spar=1)
};
.np.23 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE, w=metric@weights,
                          all.knots=TRUE, spar=1)
};
.np.24 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE, w=metric@weights,
                          all.knots=FALSE, spar=0.5)
};
.np.25 <- function(metric, transformation.x, transformation.y, metric.transformed, q) {
  if(is.null(metric@weights)) { return(NULL); }
  regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                          protected=FALSE, w=metric@weights,
                          all.knots=TRUE, spar=0.5)
};


# get the protected splines
.protected <- c(.p.00, .p.01, .p.02, .p.03, .p.04, .p.05, .p.06, .p.07, .p.08, .p.09,
                .p.10, .p.11, .p.12, .p.13, .p.14, .p.15, .p.16, .p.17, .p.18, .p.19,
                .p.20, .p.21, .p.22, .p.23, .p.24, .p.25, .p.26, .p.27);


#' @title Get the Default Protected Spline Fitters
#' @description Get the default fitters for protected splines, i.e., splines
#'   which will remain constant before and after the first and last data point,
#'   respectively.
#' @return the list of default protected splines
#' @export regressoR.spline.protected
regressoR.spline.protected <- function() .protected


# the set of default spline fitters
.default <- unlist(c(.protected,
                     .np.00, .np.01, .np.02, .np.03, .np.04, .np.05, .np.06, .np.07, .np.08, .np.09,
                     .np.10, .np.11, .np.12, .np.13, .np.14, .np.15, .np.16, .np.17, .np.18, .np.19,
                     .np.20, .np.21, .np.22, .np.23, .np.24, .np.25, .np.26, .np.27));

#' @title Get the Default Spline Fitters
#' @description Get the default fitters for splines.
#' @return the list of default splines
#' @export regressoR.spline.default
regressoR.spline.default <- function() .default
