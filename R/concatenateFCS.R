#' Merge .fcs files in selected folder
#'
#' @description
#' Function to merge .fcs files in a given folder. 
#'
#' @param fcsDir Directory with .fcs files to be combined
#' @param filename File name to create on disk
#' @param path Path of the directory to save plot to: `path` and `filename` are
#' combined to create the fully qualifed file name. Defaults to `fcsDir`
#' 
#' @return A character vector of the file name.
#'
#' @keywords merge .fcs files 
#'
#' @examples 
#' concatenateFCS(fcsDir = system.file("extdata", package="cytofast"), 
#'                filename = merged/cytofastData_merged.fcs)
#'
#' @import flowCore 
#'
#' @export
concatenateFCS <- function(fcsDir, filename, path=NULL){
  
  fcsFiles <- list.files(fcsDir, pattern=".fcs", full.names = TRUE)
  
  if(!is.null(path)){
    filename <- file.path(path, filename)
  } else {
    filename <- file.path(fcsDir, filename)
  }
  
  cellExpr <- data.frame()
  
  for(file in fcsFiles){
  message(paste("Reading .FCS file:", file))
    FCSData <- read.FCS(file,
                        transformation=FALSE,
                        truncate_max_range=FALSE)
    cellExpr <- rbind(cellExpr, FCSData@exprs)
    
  }
  parameters <- data.frame(name=FCSData@parameters@data$name,
                           desc=FCSData@parameters@data$desc,
                           range=apply(apply(cellExpr,2,range),2,diff),
                           minRange=apply(cellExpr,2,min),
                           maxRange=apply(cellExpr,2,max),
                           stringsAsFactors = FALSE)
  message("Concatenate and save new file")
  
  cData <- new("flowFrame",
               exprs = as.matrix(cellExpr),
               parameters = Biobase::AnnotatedDataFrame(parameters))
  
  write.FCS(cData, filename)
}