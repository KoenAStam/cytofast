#' preform t.test on cfList
#'
#'
#' @description Performs a  t test on cell counts slot, wrapper function thing
#'
#' @param cfList a \code{\link[cytofast]{cfList}} object. It should contain at least data in the 'counts' slot.
#' @param group one of:
#' * a character vector referring to a column name in the `samples` slot of the `cfList`.
#' * a factor indicating the grouping for the t.test.
#' @param p.adjust.method correction method used in \code{\link[stats]{p.adjust}}, choose from
#' `c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")`. If omitted, no correction takes place.
#' @param ... further arguments passed on to \code{\link[stats]{t.test}}.
#'
#'
#'
#'
function(cfList, group, p.adjust.method,  ...){

 if(is.null(cfList$counts)){
   stop("\"cfList\" does not contain a counts slot")
 }

 X <- cfList$counts
 pval <- numeric(ncol(X))

 for(i in 1:ncol(X)){
   pval[i] <- t.test(group ~ X[,i], paired=paired)
 }

 if(!missing(p.adjust.method)){
   pval <- p.adjust(pval, p.adjust.method = p.adjust.method)
 }
}


