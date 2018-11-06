#' The S4 class cfList
#'
#' @description Creates a cfList S4 object.
#' 
#' @slot samples data frame containing information on each sample
#' @slot expr data frame containing the marker expression
#' @slot counts data frame containing (normalized) cell counts per cluster and per sample
#' @slot results data frame containing results 
#'
#' @return The function returns an object of class \code{cfList}.
#'
#' @keywords cytofast, list, cf
#'
#' @import methods
#' @importFrom Rdpack reprompt
#'
#' @examples 
#' # Create an empty cfList
#' cfData <- new("cfList")
#' 
#' @export
cfList <- setClass("cfList", slots = c(samples = "data.frame", 
                                       expr = "data.frame", 
                                       counts = "data.frame",
                                       results = "data.frame"))

#' show method for cfList
#'
#' @param object a cfList object
#'
#' @return None, but prints the str() of an cfList.
#'
#' @importFrom utils str
#'
#' @examples 
#' # Create empty cfList
#' cfData <- cfList()
#' 
#' # show will return the str() of an cfList
#' cfData
#'
#' @rdname show
#' @export
setMethod("show", "cfList", function(object){
  show(str(object))
  }
)



