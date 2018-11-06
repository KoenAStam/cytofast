#' The S4 class cfList
#'
#' @description The cfList S4 object.
#' 
#' @import methods
#' 
#' @slot samples data frame containing information on each sample
#' @slot expr data frame containing the marker expression
#' @slot counts data frame containing (normalized) cell counts per cluster and per sample
#' @slot results data frame containing results 
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



