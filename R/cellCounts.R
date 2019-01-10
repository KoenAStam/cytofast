#' Extract cell counts from cfList
#'
#' @description A function to add the frequency (or abundance) of cell clusters per sample to a
#'`cfList`.
#'
#' @param cfList a cfList object. It should contain at least data in the 'expr' slot.
#' @param frequency one of
#' * a logical value. if `FALSE`, the abundance of the cell counts are used. If
#' `TRUE`, the frequency of the total amount of given cells is used.
#' * a numeric vector of same length as amount of samples
#' @param scale a logical value. Do the cell frequencies need to be centered and scaled? The
#' default \code{\link[base]{scale}} function is called.
#'
#' @return Returns the given cfList with a 'counts' slot.
#'
#' @notes There are several ways to look at the frequency of the created clusters per sample. First, one
#' could look at the abundance of the cells (frequency = `FALSE`). Second, the frequency of each cluster given 
#' as a percentage of total cells of a sample. For example, if the `cfList` is a collection of CD4+ T cells clusters
#' and one specifies frequency = `TRUE`, then the percentage of each cluster of the total CD4+ T cells is returned. 
#' This is done for each sample separately. Lastly, if there is a specific frequency the user wants to look at it is 
#' possible to specify a numeric vector that is treated as the total amount of cells to divide by (e.g. total CD45+ cells). 
#' Make sure that with the latter option specify a numeric vector of same length an order as the sampleID in `cfList@samples`.
#' 
#' The rarity of clusters can vary greatly between each other. One cluster can make up a very large chunk of the total,
#' whereas some clusters only contain a few cells. To equalize the importance of these clusters and make them more
#' comparable (e.g. in the heatmaps), one could choose to scale the data. The default \code{\link[base]{scale}} is called, which
#' both centers the data to mean zero and scales to unit variance.
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
  if(length(cfList@expr$clusterID) == 0){
    stop("clusterID is missing from `expr` slot")
  }
  if(length(cfList@expr$sampleID) == 0){
    stop("sampleID is missing from `expr` slot")
  }

  # Retrieve sampleID and clusterID
  clusterID <- factor(cfList@expr$clusterID)
  sampleID <- factor(cfList@expr$sampleID)

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
    counts <- scale(counts)
  }

  # Add to cfList
  cfList@counts <- data.frame(counts, check.names=FALSE)
  return(cfList)
}
