# make the default list
#' @include smoothSpline.R
#' @include linear.R
.makeDefault <- function(protected) {
  protected <- force(protected);

    res <- c(
           # the linear trend or constant function
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.trend.linear(metric, transformation.x, transformation.y, metric.transformed,
                                    protected=protected),
           # the weighted linear trend or constant function
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.trend.linear(metric, transformation.x, transformation.y, metric.transformed,
                                    protected=protected, weights=weights)
           },

           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                      protected=protected,
                                      all.knots=FALSE),

           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=TRUE),


           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=FALSE, df=length(metric@x)),
           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=TRUE, df=length(metric@x)),

           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=FALSE, df=max(1L, round(0.1*length(metric@x)))),
           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=TRUE, df=max(1L, round(0.1*length(metric@x)))),

           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=FALSE, df=max(1L, round(0.5*length(metric@x)))),
           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=TRUE, df=max(1L, round(0.5*length(metric@x)))),

           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=FALSE, spar=1),
           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=TRUE, spar=1),

           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=FALSE, spar=0.1),
           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=TRUE, spar=0.1),

           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=FALSE, spar=0.5),
           # spline
           function(metric, transformation.x, transformation.y, metric.transformed, q)
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected,
                                     all.knots=TRUE, spar=0.5),

           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=FALSE)
           },

           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=TRUE)
           },


           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=FALSE, df=length(metric@x))
           },
           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=TRUE, df=length(metric@x))
           },

           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=FALSE, df=max(1L, round(0.1*length(metric@x))))
           },
           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=TRUE, df=max(1L, round(0.1*length(metric@x))))
           },

           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=FALSE, df=max(1L, round(0.5*length(metric@x))))
           },
           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=TRUE, df=max(1L, round(0.5*length(metric@x))))
           },

           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=FALSE, spar=1)
           },
           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=TRUE, spar=1)
           },

           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=FALSE, spar=0.1)
           },
           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=TRUE, spar=0.1)
           },

           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=FALSE, spar=0.5)
           },
           # weighted spline
           function(metric, transformation.x, transformation.y, metric.transformed, q) {
             if(is.null(metric@weights)) { return(NULL); }
             regressoR.spline.smooth(metric, transformation.x, transformation.y, metric.transformed,
                                     protected=protected, w=metric@weights,
                                     all.knots=TRUE, spar=0.5)
           });


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
