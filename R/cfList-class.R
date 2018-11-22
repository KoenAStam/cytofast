#' @import methods
#' @import FlowSOM
NULL

#' Class cfList
#'
#' @description This is an S4 class that contains all the data and results 
#' for the cytofast workflow. Most functions in this package will both 
#' use and return a \code{cfList}.
#' 
#' @details This class is used througout the cytofast workflow, most functions in this 
#' package will both use and return a \code{cfList}. FCS files created by 
#' Cytosplore can be easily read in with \code{\link{readCytosploreFCS}}) and will
#' be returned as a cfList. It is also possible to manually create instances of this 
#' class if another clustering method is preferred. Below an example is 
#' shown for \code{\link[FlowSOM]{FlowSOM}}. 
#' 
#' @slot samples data frame containing all meta information on each sample
#' @slot expr data frame containing the marker expression
#' @slot counts data frame containing (standardized) cell counts per cluster and per sample
#' @slot results list containing any results 
#' 
#' @section Methods:
#' \describe{
#' \item{\code{samples}}{a method for obtaining the samples data}
#' \item{\code{expr}}{a method for obtaining the marker expression}
#' \item{\code{counts}}{a method for obtaining the cell counts}
#' \item{\code{results}}{a method for obtaining any results from performed tests}
#' }
#' 
#' @examples
#' ### manually create instance of cfList class
#' library(FlowSOM)
#' 
#' ## Cluster with FlowSOM
#' dirFCS <- system.file("extdata", package="cytofast")
#' fSOM <- FlowSOM(input = dirFCS, 
#'                 transform = FALSE,
#'                 scale = FALSE,
#'                 colsToUse = c(9:11, 15:52),
#'                 nClus = 10, # Note that this is an ambiguous choice
#'                 seed = 123)
#' 
#' ## expr slot
#' # retrieve clusters
#' clusterID <- as.factor(fSOM$FlowSOM$map$mapping[,1])
#' levels(clusterID) <- fSOM$metaclustering
#' 
#' # retrieve samples (As example, we assume each FCS file is its own sample)
#' sampleID <- lapply(fSOM$FlowSOM$metaData, function(x){rep(x[1], each = length(x[1]:x[2]))})
#' attr(sampleID, 'names') <- NULL
#' sampleID <- as.factor(unlist(sampleID)) 
#' levels(sampleID) <- paste("ID", 1:10, sep="_")
#' 
#' exprD <- data.frame(clusterID, 
#'                     sampleID, 
#'                     fSOM$FlowSOM$data[, c(9:11, 15:52)])
#' 
#' ## samples slot
#' samplesD <- data.frame(sampleID = levels(sampleID), 
#'                        group = rep(c("group1", "group1"), each=5)) 
#'
#' ## create cfList
#' cfList(samples = samplesD,
#'        expr = exprD)
#' 
#' @name cfList-class
#' @rdname cfList-class
#' 
#' @export cfList
#' @exportClass cfList
cfList <- setClass("cfList", slots = c(samples = "data.frame", 
                             expr = "data.frame", 
                             counts = "data.frame",
                             results = "list"))

setValidity("cfList", function(object){
  valid <- TRUE
  msg <- NULL
  
  ### validity of samples
  if(length(object@samples$sampleID) == 0){
    valid <- FALSE
    msg <- c(msg, "samples slot is missing a \"sampleID\" column")
  }
  
  ### validity of expr
  if(length(object@expr$sampleID) == 0){
    valid <- FALSE
    msg <- c(msg, "expr slot is missing a \"sampleID\" column")
  }
  
  if(length(object@expr$clusterID) == 0){
    valid <- FALSE
    msg <- c(msg, "expr slot is missing a \"clusterID\" column")
  }
  
  ### validity of counts
  
  ### validity of results
  
  ### general checks
  #if(levels(object@expr$sampleID) != levels(object@samples$sampleID)){
  #  valid <- FALSE
  #  msg <- c(msg, "levels from \"sampleID\" do not correspond between
  #           samples and expr slot")
  #}
  
  ### message
  if(valid) TRUE else msg
  
})

