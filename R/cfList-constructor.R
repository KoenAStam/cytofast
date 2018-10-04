#' Constructor for a cfList
#'
#' @description Creates a \code{cfList} S3 object.
#'
#' @param expr data frame containing the marker expression
#' @param counts data frame containing (normalized) cell counts per cluster and per sample
#' @param samples data frame containing information on each sample
#'
#' @return The function returns an object of class \code{cfList}.
#'
#' @keywords cytofast, list, cf
#'
#' @importFrom Rdpack reprompt
#'
#' @export
cfList <- function(samples=NULL, expr=NULL, counts=NULL){

  x <- list(samples = samples, expr = expr, counts = counts)
  class(x) <- "cfList"

  #if(missing(counts)){
    # cat("agrument \"counts\" is missing, using default `cellCounts`")
  #  x <- cellCounts(x)
  #}

  x
}

#' Print method for cfList
#'
#' @param x a cfList object
#' @param ... further arguments passed to or from other methods
#'
#' @importFrom utils str
#'
#' @rdname print
#' @export
print.cfList = function(x, ...) {
  print(str(x), ...)
}

