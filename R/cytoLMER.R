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
library(cytofast)
library(lmerTest)

# Read Data
dirFCS <- system.file("extdata", package="cytofast")
cfData <- readCytosploreFCS(dir = dirFCS, colNames = "description")

# relabeling of clusterID
levels(cfData@expr[,"clusterID"]) <- gsub("[^0-9]", "", levels(cfData@expr[,"clusterID"]))

# Add cell counts to cfList and add meta data
cfData <- cellCounts(cfData, frequency = TRUE, scale = TRUE)
meta <- spitzer[match(row.names(cfData@samples), spitzer$CSPLR_ST),]
cfData@samples <- cbind(cfData@samples, meta)

# Set grouping variable
cfData@samples$effect <- gsub("_D\\d", "", spitzer$group)