#' Show cfList object
#'
#' @param object a cfList object
#' 
#' @return Prints a light version of a `cfList` to the console
#' 
#' 
#' 
setMethod("show", "cfList", function(object){
  cat("An object of class \"cfList\" \n\n")
  
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
    print(object@counts)
    cat("\n")
  }
  
  # show resuls
  if(!length(object@results) == 0){
    print(object@results)
  }
  
})

#' Extract parts of cfList
#' 
#' @param x a cfList object from which to extract from
#' @param i index specifying which samples to extract. The index is either
#' a \code{numeric} or \code{character} vector. 
#' @param j index specifying which clusters to extract. The index is either
#' a \code{numeric} or \code{character} vector. 
#' 
#' @return Returns the specified clusters or samples from a `cfList`. 
#' 
#' 
setMethod("[", signature(x = "cfList"), function (x, i, j){
  
  # function
  print("void function")
  }
)

