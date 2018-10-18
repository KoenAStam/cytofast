#' Draw a density (median signal intensity) plot
#'
#' @description
#' Function to plot the density (median signal intensity) for given markers.
#'
#' @param cfList a \code{\link[cytofast]{cfList}} object. It should contain at least data in the 'expr' and 'samples' slots.
#' @param markers character vector with column names of the markers to be plotted. A numeric vector
#' is also accepted, note that 1 starts after removing columns 'clusterID' and 'sampleID'.
#' @param byGroup a character, referring to a column name in the `samples` slot of the `cfList`. This will
#' be used as the grouping for the y-axis.
#' @param byCluster character vector specifying which cluster to be plotted. This will be used as
#' the grouping for the y-axis.
#' @param ... Additional arguments passed on to \code{\link[ggridges]{geom_density_ridges}}.
#'
#'
#' @keywords median signal intensity density
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom methods is
#'
#' @export

msiPlot <- function(cfList, markers, byGroup = NULL, byCluster = NULL, ...){

  if(!is(cfList, "cfList")){
    stop("first argument is not of class \"cfList\"")
  }

  if(length(cfList$expr$clusterID) == 0){
    stop("clusterID is missing from `expr` slot")
  }

  if(length(cfList$expr$clusterID) == 0){
    stop("sampleID is missing from `expr` slot")
  }

  if(is(markers, "character")){
    if(!all(markers %in% colnames(cfList$expr))){
      stop("\"markers\" contains a character that's not in the `expr` slot")
    }
  }

  if(missing(byGroup) && missing(byCluster)){
    stop("neither \"byGroup\" or \"byCluster\" is specified, choose one")
  }

  if(!missing(byGroup) && !missing(byCluster)){
    stop("\"byGroup\" and \"byCluster\" are both specified, choose one")
  }

  X <- cfList$expr[,!colnames(cfList$expr) %in% c("clusterID", "sampleID")][,markers, drop=F]
  clusterID <- as.factor(cfList$expr$clusterID)
  sampleID <- as.factor(cfList$expr$sampleID)

  if(all(unlist(lapply(X, is.numeric))) == FALSE){
    stop("Not all markers in 'expr' slot are numeric")
  }

  if(is(byGroup, "character") && length(byGroup) == 1){
    if(byGroup %in% colnames(cfList$samples)){
      groups <- sampleID
      levels(groups) <- factor(cfList$samples[,byGroup])
      X <- data.frame(X, groups, check.names=F)
      legendtitle <- "groups"
    } else {
        stop("\"group\" is a character, but is missing from `samples` slot")
      }
  }

  if(is(byCluster, "character")){
    if(all(byCluster %in% clusterID)){
      X <- data.frame(X[clusterID %in% byCluster,],
                      groups = factor(clusterID[clusterID %in% byCluster], levels=byCluster))
      legendtitle <- "clusters"
      } else {
        stop("Not all elements of \"byCluster\" correspond with clusterID in `expr` slot")
      }
  } else if(!missing(byCluster)) {
    stop("\"byCluster\" is not of class character")
  }

  plotData <- melt(X, id.vars="groups")

  g1 <- ggplot(data=plotData, aes_string(x="value", y="groups", color="groups", fill="groups"))
  g1 + ggridges::geom_density_ridges(alpha=0.3, ...) +
       scale_x_continuous(breaks=round(min(plotData$value)): round(max(plotData$value))) +
       scale_y_discrete(expand = c(0,0)) +
       ylab(legendtitle) +
       xlab("expression") +
       facet_wrap(~ variable) +
       guides(fill = guide_legend(title=legendtitle),
              colour = "none") +
       theme_ridges()
}





