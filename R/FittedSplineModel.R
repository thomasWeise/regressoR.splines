#' @title A Spline Model
#' @description This class holds a spline model.
#' @slot name the name of the spline approach
#' @return an instance of \code{\link{FittedSplineModel}}
#' @exportClass FittedSplineModel
#' @importClassesFrom regressoR.base FittedModel
#' @importFrom methods setClass representation
#' @seealso FittedSplineModel.new
FittedSplineModel <- setClass(
  Class = "FittedSplineModel",
  contains = "FittedModel",
  representation = representation(name="character"),
  validity = function(object) {
    # check the name
    if((!is.character(object@name)) ||
       (length(object@name) != 1L)  ||
       (nchar(object@name) <= 0L)) {
      return("Invalid name.");
    }
    return(TRUE);
  }
)


#' @title Create a New Instance of \code{\link{FittedSplineModel}}.
#' @description Create a New Instance of \code{\link{FittedSplineModel}}.
#' @param f a function accepting one parameter and returning a value
#' @param quality the quality of the model on the original data, computed by a
#'   quality metric, smaller values are better
#' @param size the size of the model, i.e., the number of parameters
#' @param name the name of the spline model
#' @return the new instance
#' @importFrom methods new validObject
#' @export FittedSplineModel.new
FittedSplineModel.new <- function(f, quality, size, name) {
  result <- new("FittedSplineModel", f=f, quality=quality, size=size,
                name=name);
  result <- force(result);
  result@f <- force(result@f);
  result@name <- force(result@name);
  result@quality <- force(result@quality);
  result@size <- force(result@size);
  result <- force(result);
  validObject(result);
  return(result);
}

#' @title Convert a \code{FittedSplineModel} to a String
#' @description well, convert a \code{FittedSplineModel} to a String
#' @param x the \code{FittedSplineModel}
#' @return the string
#' @importFrom utilizeR functionToString
#' @export FittedSplineModel.as.character
FittedSplineModel.as.character <- function(x) x@name

#' @title Convert a \code{\link{FittedSplineModel}} to a String
#' @description the \code{as.character} implementation for
#'   \code{\link{FittedSplineModel}}
#' @param x the object
#' @return the name of the object
#' @importFrom methods setMethod
#' @name as.character
#' @aliases as.character,FittedSplineModel-method
methods::setMethod("as.character", "FittedSplineModel", FittedSplineModel.as.character)
