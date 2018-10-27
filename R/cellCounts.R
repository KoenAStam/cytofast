#' Extract cell counts from cfList
#'
#' @description A function to add the frequency (or abundance) of cell clusters per sample to a
#'`cfList`.
#'
#' @param cfList a \code{\link[cytofast]{cfList}} object. It should contain at least data in the 'expr' slot.
#' @param frequency one of:
#' * a logical value. if `FALSE`, the abundance of the cell counts are used. If
#' `TRUE`, the frequency of the total amount of given cells is used.
#' * a numeric vector of same length as amount of samples
#' @param scale a logical value. Do the cell frequencies need to be centered and scaled? The
#' default \code{\link[base]{scale}} function is called.
#'
#' @keywords counts, frequency, cells
#'
#' @importFrom stats median
#'
#' @examples
#' # Read Data
#' dirFCS <- system.file("extdata", package="cytofast")
#' cfData <- readCytosploreFCS(dir = dirFCS, colNames = "description")
#'
#' # Add cell counts to cfList
#' cfData <- cellCounts(cfData)
#'
#'
#' @export

cellCounts <- function(cfList, frequency = FALSE, scale = FALSE){

  if(!is(cfList, "cfList")){
    stop("first argument is not of class \"cfList\"")
  }
  if(length(cfList$expr$clusterID) == 0){
    stop("clusterID is missing from `expr` slot")
  }
  if(length(cfList$expr$clusterID) == 0){
    stop("sampleID is missing from `expr` slot")
  }

  # Retrieve sampleID and clusterID
  clusterID <- factor(cfList$expr$clusterID)
  sampleID <- factor(cfList$expr$sampleID)

  # Create counts table
  counts <- as.data.frame.matrix(table(sampleID, clusterID))

  if(is(frequency, "numeric")){
    if(length(frequency) == nrow(counts)){
      counts <- apply(counts, 2, "/", frequency)
    } else {
        stop("\"frequency\" is numeric, but not of same length as samples")
      }
  }

  if(is(frequency, "logical")){
    if(frequency == TRUE){
      counts <- apply(counts, 2, "/", rowSums(counts))
    }
  }

  if(scale){
    counts <- data.frame(scale(counts), check.names=FALSE)
  }

  # Add to cfList
  cfList$counts <- counts
  return(cfList)
}
