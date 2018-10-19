#' preform t-test on cfList
#'
#'
#' @description Performs a separate t-test on each cluster within a cfList. The output is added and can also be used
#' by other functions
#'
#' @param cfList a \code{\link[cytofast]{cfList}} object. It should contain at least data in the 'counts' slot.
#' @param group one of:
#' * a character vector referring to a column name in the `samples` slot of the `cfList`.
#' * a factor indicating the grouping for the t.test.
#' @param adjustMethod  character, correction method used in \code{\link[stats]{p.adjust}}, choose from
#' `c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")`. If omitted, no correction takes place.
#' @param ... further arguments passed on to \code{\link[stats]{t.test}}.
#'
#'
#' @importFrom stats t.test p.adjust
#' @importFrom methods is
#'
#'
#' @export
cytottest <- function(cfList, group, adjustMethod,  ...){


  if(!is(cfList, "cfList")){
    stop("first argument is not of class \"cfList\"")
  }

  if(is.null(cfList$counts)){
     stop("\"cfList\" does not contain a counts slot")
  }

  if(is(group, "character") && length(group) == 1){
    if(group %in% colnames(cfList$samples)){
      grouping <- factor(cfList$samples[,group])
    } else {
      stop("\"group\" is a character, but is missing from `samples` slot ")
    }
  }
  if(is(group, "factor") && length(group) == nrow(cfList$counts)){
    grouping <- group
  }

  X <- cfList$counts
  results <- data.frame(matrix(0, nrow=ncol(X), ncol=3))

  for(i in seq_len(ncol(X))){
     test <- t.test(X[,i] ~ grouping, ...)
     results[i, 3] <- test$p.value
     results[i, 1:2] <- test$estimate
  }

  colnames(results) <- c(paste("mean", levels(grouping)[1], sep="_"),
                         paste("mean", levels(grouping)[2], sep="_"),
                         "pvalue")
  results <- cbind(clusters = colnames(X), results)

  if(!missing(adjustMethod)){
     results$adjusted <- p.adjust(results[,"pvalue"], method = adjustMethod)
  }

  cfList$results <- results
  cfList
}




