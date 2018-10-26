#' Draw boxplots for cfList
#'
#' @description Draw boxplots for all given clusters. Values are based on the `counts` slot in the
#' `cfList`.
#'
#' @param cfList a \code{\link[cytofast]{cfList}} object. It should contain at least data in the 'counts' slot.
#' @param group one of:
#' * a character referring to a column name in the `samples` slot of the `cfList`.
#' * a factor indicating the grouping (x-axis) for the boxplots.
#' @param stat ignore, will be implemented soon.
#'
#' @keywords boxplot,
#'
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats median hclust dist
#' @importFrom methods is
#'
#' @export

cytoBoxplots <- function(cfList, group, stat){

  if(!is(cfList, "cfList")){
    stop("first argument is not of class \"cfList\"")
  }

  if(is.null(cfList$counts)){
    stop("`counts` slot is missing from \"cfList\"")
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

  plotData <- reshape2::melt(cbind(grouping, data.frame(cfList$counts, check.names=FALSE)), id.vars=1)
  plotData$variable <- factor(plotData$variable, levels=levels(plotData$variable))

  g1 <- ggplot(plotData, aes_string(x="grouping", y="value", color="grouping"))
  g1 + geom_boxplot() +
       geom_point() +
       facet_wrap(. ~ variable, scales = "free") +
       theme_bw() +
       theme(text = element_text(size = 8), axis.text.x = element_blank())
}
