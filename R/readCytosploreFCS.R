#' Read .fcs files created by cytosplore
#'
#' @description
#' This function reads and combines .FCS files created specifcally by Cytosplore.
#'
#' @param dir directory containing the .FCS files created by Cytosplore
#' @param colNames character string that specifies which label should be used as the column names. This
#' could be the name of the markers 'names' or the description for the markers 'description'.
#'
#' @return The function returns an object of class \code{\link[cytofast]{cfList}}. 
#'
#' @note This function is a wrapper around \code{\link[flowCore]{read.FCS}}. For more
#' flexibility see their help page.
#'
#' @keywords read, data, FCS, cytosplore
#'
#' @importFrom flowCore read.FCS
#' @import Rdpack
#'
#' @examples
#' dirFCS <- system.file("extdata", package="cytofast")
#' cfData <- readCytosploreFCS(dir = dirFCS, colNames = "description")
#'
#' @export

readCytosploreFCS <- function(dir=NULL, colNames = c("names", "description")){

  # Error catching for dir
  if(is.character(dir) == FALSE){
    stop("directory is not a character string")
  } else if(dir.exists(dir) == FALSE) {
    stop("directory does not exist")
  }

  # Error catching for colNames
  if(missing(colNames)){
    warning("colNames was not specified and set to 'description'")
    colNames <- "description"
  } else if(!colNames %in% c("names", "description")){
    warning("colNames was not correctly specified and set to 'description'")
    colNames <- "description"
  }

  # Path and names of FCS files
  FCSFilePaths <- list.files(path=dir, pattern=".fcs", full.names=TRUE)
  FCSFileNames <- list.files(path=dir, pattern=".fcs", full.names=FALSE)
  if(length(FCSFileNames) == 0){
    stop("there are no .fcs files in this directory")
  }

  # Loop through files and read into R environment
  x <- data.frame()
  for(i in seq_along(FCSFilePaths)){
    cat(paste("Reading .FCS cluster:",i,"\n"))

    FCSfile <- flowCore::read.FCS(FCSFilePaths[i],
                        transformation=FALSE,
                        truncate_max_range=FALSE)

    # Only keep expression
    exprs <- FCSfile@exprs

    # Change column names to description
    if(colNames == "description"){
      colnames(exprs) <- FCSfile@parameters@data$desc
    }

    x <- rbind(x, data.frame(clusterID=FCSFileNames[i], exprs))
  }

  # Make sampletag a factor and rename to sampleID
  x$CSPLR_ST <- as.factor(x$CSPLR_ST)
  x$clusterID <- as.factor(x$clusterID)
  colnames(x)[colnames(x) == 'CSPLR_ST'] <- 'sampleID'

  # change order and return cfList
  x <- x[,c("clusterID", "sampleID", setdiff(colnames(x), c("clusterID", "sampleID")))]
  output <- new("cfList", samples = data.frame(sampleID = levels(x$sampleID),
                                               row.names = levels(x$sampleID)),
                          expr = x)
  return(output)

}


