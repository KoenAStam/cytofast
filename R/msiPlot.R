#' Draw a density (median signal intensity) plot
#'
#' @description
#' Function to plot the density (median signal intensity) for given markers.
#'
#' @param cfList a \code{\link[cytofast]{cfList}} object. It should contain at least data in the 'expr' and 'samples' slots.
#' @param markers character vector with column names of the markers to be plotted. A numeric vector
#' is also accepted, note that 1 starts after removing columns 'clusterID' and 'sampleID'.
#' @param clusters character vector with names of the clusters to be plotted.
#' @param group a character referring to a column name in the `samples` slot of the `cfList`.
#' @param ... Additional arguments passed on to \code{\link[ggridges]{geom_density_ridges}}
#'
#'
#' @keywords median signal intensity density
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom ggridges geom_density_ridges theme_ridges
#'
#' @export

msiPlot <- function(cfList, markers, clusters, group, ...){

  if(class(cfList) != "cfList"){
    stop("first argument is not of class \"cfList\"")
  }
  if(length(cfList$expr$clusterID) == 0){
    stop("clusterID is missing from `expr` slot")
  }
  if(length(cfList$expr$clusterID) == 0){
    stop("sampleID is missing from `expr` slot")
  }
  if(!all(markers %in% colnames(cfList$expr))){
    stop("\"markers\" contains a character that's not in the `expr` slot")
  }

  X <- cfList$expr[,!colnames(cfList$expr) %in% c("clusterID", "sampleID")][,markers, drop=F]
  clusterID <- as.factor(cfList$expr$clusterID)
  sampleID <- as.factor(cfList$expr$sampleID)

  if(all(unlist(lapply(X, is.numeric))) == FALSE){
    stop("Not all markers in 'expr' slot are numeric")
  }

  if(class(group) == "character" & length(group) == 1){
    if(group %in% colnames(cfList$samples)){
      groups <- sampleID
      levels(groups) <- factor(cfList$samples[,group])
      X <- data.frame(X, groups, check.names=F)
    } else {
      stop("\"group\" is a character, but is missing from `samples` slot ")
    }
  }

  if(!missing(clusters)){
    if(class(clusters) == "character"){
      X <- X[clusterID %in% clusters,]
    } else {
      stop("\"clusters\" is not a character")
    }
  }

  plotData <- melt(X, id.vars="groups")
  g1 <- ggplot(data=plotData, aes_string(x="value", y="groups", color="groups", fill="groups"))
  g1 + ggridges::geom_density_ridges(alpha=0.3, ...) +
       scale_x_continuous(breaks=round(min(plotData$value)): round(max(plotData$value))) +
       scale_y_discrete(expand = c(0,0)) +
       facet_wrap(~ variable) +
       theme_ridges()
}


