#'@import methods
#'@include cfList-class.R cfList-accessors.R
NULL

#' Show cfList object
#'
#' @rdname cfList-class
#' @aliases show,cfList-method
#' @usage NULL
setMethod("show", "cfList", function(object){
  cat("An object of class cfList \n\n")
  
  # show header samples
  if(!length(object@samples) == 0){
    nr <- min(5, nrow(object@samples))
    nc <- min(5, ncol(object@samples))
    cat("@samples \n")
    print(object@samples[1:nr, 1:nc, drop=FALSE])
    cat(nrow(object@samples), "rows and", ncol(object@samples),"columns \n\n")
  }
  
  # show header of expr
  if(!length(object@expr) == 0){
    nr <- min(5, nrow(object@expr))
    nc <- min(5, ncol(object@expr))
    cat("@expr \n")
    print(object@expr[1:nr, 1:nc])
    cat(nrow(object@expr), "rows and", ncol(object@expr),"columns \n\n")
  }
  
  # show counts
  if(!length(object@counts) == 0){
    cat("@counts \n")
    print(object@counts)
    cat("\n")
  }
  
  # show resuls
  if(!length(object@results) == 0){
    cat("@results \n")
    print(object@results)
  }
  
})

#' Extract parts of cfList
#' 
#' @param x a cfList object from which to extract from
#' @param i index specifying which samples to extract. The index is either
#' a \code{numeric} or \code{character} vector. 
#' @param j index specifying which clusters to extract. The index must be
#' a \code{character} vector. 
#' 
#' @return Returns the specified clusters or samples from a `cfList`. 
#' 
setMethod("[", signature(x = "cfList"), function (x, i, j){
  
  # Initialize
  sampleID <- levels(factor(x@expr$sampleID))
  clusterID <- levels(factor(x@expr$clusterID))
  
  # get sampleID
  if(!missing(i)){
    if(is.numeric(i) == TRUE){
      sampleID <- as.character(x@samples[i, "sampleID"])
    } else if(is.character(i) == TRUE){
      sampleID <- as.character(x@samples[which(x@samples$sampleID %in% i), "sampleID"])
    } else {
      message("i is neither a numeric or character")
    }
    if(length(sampleID) == 0){
      stop(i, " not found in sampleID")
    }
  }
  
  # get clusterID
  if(!missing(j)){
    if(is.character(j) == TRUE){
      clusterID <- as.character(x@expr[which(x@expr$clusterID %in% j), "clusterID"])
    } else {
      message("j is not a character")
    }
    if(length(clusterID) == 0){
      stop(j, " not found in clusterID") 
    }
  }
  
  # return cfList
  cfList(samples = x@samples[x@samples$sampleID %in% sampleID,],
         expr = x@expr[x@expr$sampleID %in% sampleID & x@expr$clusterID %in% clusterID,])

})




