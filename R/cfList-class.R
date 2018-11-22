#' An S4 class cfList
#'
#' Class \code{cfList} for most cytofast functions
#' 
#' @slot samples data frame containing information on each sample
#' @slot expr data frame containing the marker expression
#' @slot counts data frame containing (normalized) cell counts per cluster and per sample
#' @slot results data frame containing results 
#' 
#' @import methods
#' 
#' @name cfList-class
#' @rdname cfList-class
#' @exportClass cfList
setClass("cfList", slots = c(samples = "data.frame", 
                             expr = "data.frame", 
                             counts = "data.frame",
                             results = "data.frame"))

