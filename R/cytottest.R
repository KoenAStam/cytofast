#' preform t-test on cfList
#'
#'
#' @description Performs a separate t-test on each cluster within a cfList. The output is added and can also be used
#' by other functions
#'
#' @param cfList a cfList object. It should contain at least data in the 'counts' slot.
#' @param group one of:
#' * a character vector referring to a column name in the `samples` slot of the `cfList`.
#' * a factor indicating the grouping for the t.test.
#' @param adjustMethod  character, correction method used in \code{\link[stats]{p.adjust}}, choose from
#' `c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")`. If omitted, no correction takes place.
#' @param ... further arguments passed on to \code{\link[stats]{t.test}}.
#'
#' @return Returns a cfList with a 'results' slot.
#' 
#' @examples 
#' # Read Data
#' dirFCS <- system.file("extdata", package="cytofast")
#' cfData <- readCytosploreFCS(dir = dirFCS, colNames = "description")
#' 
#' # relabeling of clusterID
#' levels(cfData@expr[,"clusterID"]) <- gsub("[^0-9]", "", levels(cfData@expr[,"clusterID"]))  
#'
#' # Add cell counts to cfList and add meta data
#' cfData <- cellCounts(cfData, frequency = TRUE, scale = TRUE)
#' meta <- spitzer[match(row.names(cfData@samples), spitzer$CSPLR_ST),]
#' cfData@samples <- cbind(cfData@samples, meta)
#' 
#' # Run t-test
#' cfData@samples$effect <- gsub("_D\\d", "", spitzer$group)
#' cfData <- cytottest(cfData, group  = "effect", adjustMethod = "bonferroni")
#' cfData@results
#' 
#' @importFrom stats t.test p.adjust
#' @importFrom methods is
#'
#'
#' @export
cytottest <- function(cfList, group, adjustMethod = c("none", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr"),  ...){

  if(!is(cfList, "cfList")){
    stop("first argument is not of class \"cfList\"")
  }

  if(is.null(cfList@counts)){
     stop("\"cfList\" does not contain a counts slot")
  }

  if(is(group, "character") && length(group) == 1){
    if(group %in% colnames(cfList@samples)){
      grouping <- factor(cfList@samples[,group])
    } else {
      stop("\"group\" is a character, but is missing from `samples` slot ")
    }
  }
  if(is(group, "factor") && length(group) == nrow(cfList@counts)){
    grouping <- group
  }

  X <- cfList@counts
  results <- data.frame(matrix(0, nrow=ncol(X), ncol=3))

  for(i in seq_len(ncol(X))){
     test <- t.test(X[,i] ~ grouping, ...)
     results[i, 3] <- test$p.value
     results[i, seq_len(2)] <- test$estimate
  }

  colnames(results) <- c(paste("mean", levels(grouping)[1], sep="_"),
                         paste("mean", levels(grouping)[2], sep="_"),
                         "pvalue")
  results <- cbind(clusters = colnames(X), results)
  
  adjustMethod <- match.arg(adjustMethod)
  if(adjustMethod != "none"){
  results$adjusted <- p.adjust(results[,"pvalue"], method = adjustMethod)
  }

  cfList@results <- results
  cfList
}




