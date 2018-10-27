#' Draw heatmaps for cfList
#'
#' @description
#' Function to draw two heatmaps. They visualize the median intensity of the markers for the
#' created clusters. The ordering of the clusters is based on the default
#' hierarchical cluster analysis \code{\link[stats]{hclust}}. Note that hclust takes the data after
#' the median intensity is calculated per cluster, thus placing the most similar clusters
#' next to each other.
#'
#' @param cfList a \code{\link[cytofast]{cfList}} object.
#' @param group one of:
#' * a character vector referring to a column name in the `samples` slot of the `cfList`.
#' * a factor indicating the grouping (x-axis) for the boxplots.
#' @param legend logical, whether a legend should be added
#'
#' @keywords heatmap, markers, FCS
#'
#' @import ggplot2
#' @import grid
#' @importFrom reshape2 melt
#' @importFrom stats median cutree hclust dist
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#'
#' @export

cytoHeatmaps <- function(cfList, group, legend = FALSE){

  if(!is(cfList, "cfList")){
    stop("first argument is not of class \"cfList\"")
  }
  if(length(cfList$expr$clusterID) == 0){
    stop("clusterID is missing from `expr` slot")
  }
  if(length(cfList$expr$clusterID) == 0){
    stop("sampleID is missing from `expr` slot")
  }

  X <- cfList$expr[,!colnames(cfList$expr) %in% c("clusterID", "sampleID")]
  clusterID <- factor(cfList$expr$clusterID)
  sampleID <- factor(cfList$expr$sampleID)

  if(all(unlist(lapply(X, is.numeric))) == FALSE){
    stop("Not all markers are numeric")
  }

  ## Heatmap1 - (markers vs clusters)

  # extract median intenstity per cluster
  dataMed <- data.frame(matrix(0, ncol=ncol(X), nrow=length(levels(clusterID))))
  for(i in seq_len(ncol(X))){
    dataMed[,i] <- tapply(X[,i], clusterID, median)
  }
  colnames(dataMed) <- colnames(X)
  dataHeat <- data.frame(t(dataMed))
  colnames(dataHeat) <- levels(clusterID)

  # hclust on medians of markers
  hc1 <- hclust(dist(dataMed))

  # prepare plotdata
  plotDat <- reshape2::melt(data.frame(markers=factor(rownames(dataHeat), levels=rev(rownames(dataHeat))),
                             dataHeat, check.names=FALSE), id.vars="markers")
  plotDat$variable <- factor(plotDat$variable, levels=levels(plotDat$variable)[hc1$order])

  ggheat1  <- ggplot(plotDat, aes_string(y="markers", x="variable", fill="value")) +
              geom_tile(colour='black', show.legend=TRUE) +
              theme_grey(base_size=9) +
              labs(x="", y="") +
              scale_fill_gradientn(colours=colorRampPalette(rev(brewer.pal(n = 11, name ="RdYlBu")))(100)) +
              scale_x_discrete(expand=c(0,0)) +
              scale_y_discrete(expand=c(0,0), position="left") +
              guides(fill = guide_colourbar(title="expression")) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, size=9),
                    axis.text.y = element_text(size=9)) + theme_void()

  ## heatmap 2 - (samples vs clusters)

  # extract cell counts per sample

  if(length(cfList$counts) == 0){
  warning("`counts` slot is missing from cfList, using default")
  dataHeat2 <- cellCounts(cfList)$counts
  } else {
    dataHeat2 <- cfList$counts
  }

  # hclust on cell counts of samples
  hc2 <- hclust(dist(dataHeat2))

  # prepare plotdata2
  plotDat2 <- reshape2::melt(data.frame(samples=factor(rownames(dataHeat2), levels=rownames(dataHeat2)[hc2$order]),
                             dataHeat2, check.names=FALSE), id.vars="samples")
  plotDat2$variable <- factor(plotDat2$variable, levels=levels(plotDat2$variable)[hc1$order])

  ggheat2 <- ggplot(plotDat2, aes_string(y="samples", x="variable", fill="value")) +
            geom_tile(colour='black', show.legend=TRUE) +
            theme_grey(base_size=9) +
            labs(x="", y="") +
            scale_fill_gradientn(colours=colorRampPalette(rev(brewer.pal(n = 11, name ="PRGn")))(100)) +
            scale_x_discrete(expand=c(0,0)) +
            scale_y_discrete(expand=c(0,0), position="left") +
            guides(fill = guide_colourbar(title="frequency")) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size=9),
                  axis.text.y = element_text(size=9)) + theme_void()


  ## heatmap 3 - (group plotting)
  if(missing(group)){
    plotDat3 <- data.frame(samples=seq_len(nrow(dataHeat2)), variable=1, value=factor(0))
  } else {
    if(is(group, "character") & length(group) == 1){
      if(group %in% colnames(cfList$samples)){
        grouping <- factor(cfList$samples[,group])
      } else {
        stop("\"group\" is missing as variable from `samples` slot")
      }
    }
    if(is(group, "factor") && length(group) == nrow(cfList$counts)){
      grouping <- group
    }
    plotDat3 <- data.frame(samples=seq_len(nrow(dataHeat2)), variable=1, value=factor(grouping)[hc2$order])
  }

  ggheat3 <- ggplot(plotDat3, aes_string(y="samples", x="variable", fill="value")) +
            geom_tile(colour="black", show.legend=TRUE) +
            scale_fill_discrete() +
            scale_x_discrete(expand=c(0,0)) +
            scale_y_discrete(expand=c(0,0), position="left") +
            guides(fill=guide_legend(title="")) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size=9),
                  axis.text.y = element_text(size=9)) + theme_void()

  ## Extract ggplot legend
  g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(vapply(X = tmp$grobs, FUN = function (x) x$name, FUN.VALUE = character(1)) == "guide-box")
    legend <- tmp$grobs[[leg]]$grobs[[1]]
    legend
  }

  ## Create grid for combining plots

  ## make grobs
  grtree1 <- ggplotGrob(drawTree(hc1))
  grtree2 <- ggplotGrob(drawTree(hc2) + coord_flip())
  grheat1 <- ggplotGrob(ggheat1 + theme(legend.position = "none"))
  grheat2 <- ggplotGrob(ggheat2 + theme(legend.position = "none"))
  grheat3 <- ggplotGrob(ggheat3 + theme(legend.position= "none"))
  grlegend1 <- g_legend(ggheat1)
  grlegend2 <- g_legend(ggheat2)
  grlegend3 <- g_legend(ggheat3)

  # viewport proportions
  totalRows <- nrow(dataHeat) + nrow(dataHeat2)
  heightHeat1 <- (nrow(dataHeat) / (totalRows)) - (0.145/2)
  heightHeat2 <- (nrow(dataHeat2) / (totalRows)) -  (0.145/2)

  # top viewport
  topVp <- viewport(layout = grid.layout(7, 5,
                                         widths = unit(c(0.10, 0.8, 0.005, 0.01, 0.085), c("npc", "null", "npc", "npc", "npc")),
                                         heights = unit(c(0.085, rep(heightHeat1/3, 3), 0.01, heightHeat2, 0.05), c("npc", "npc", "npc", "npc", "npc", "npc", "npc"))))

  # children viewports
  tree1 <- viewport(layout.pos.row = 1, layout.pos.col = 2, name="tree1")
  if(missing(group)){
    tree2 <- viewport(layout.pos.row = 6, layout.pos.col = 3:5, name="tree2")
  } else {
      tree2 <- viewport(layout.pos.row = 6, layout.pos.col = 5, name="tree2")
    }
  heat1 <- viewport(layout.pos.row = 2:4, layout.pos.col = 2, name="heat1",
                    xscale=c(0,ncol(dataHeat)), yscale=c(0,nrow(dataHeat)))
  heat2 <- viewport(layout.pos.row = 6, layout.pos.col = 2, name="heat2",
                    xscale=c(0,ncol(dataHeat)), yscale=c(0,nrow(dataHeat2)))
  heat3 <- viewport(layout.pos.row = 6, layout.pos.col=4, name="heat3",
                    xscale=c(0, 1), yscale=c(0,nrow(dataHeat)))
  legend1 <- viewport(layout.pos.row = 2, layout.pos.col=5, name="legend1")
  legend2 <- viewport(layout.pos.row = 3, layout.pos.col=5, name="legend2")
  legend3 <- viewport(layout.pos.row = 4, layout.pos.col=5, name="legend3")

  # create plot
  grid.newpage()
  finalPlot <- vpTree(topVp, vpList(tree1, tree2, heat1, heat2, heat3, legend1, legend2, legend3))
  pushViewport(finalPlot)

  # add components
  seekViewport("tree1")
  grid.draw(grtree1)

  seekViewport("tree2")
  grid.draw(grtree2)

  seekViewport("heat1")
  grid.draw(grheat1)
  grid.yaxis(seq(0.5, nrow(dataHeat) - 0.5, length.out=nrow(dataHeat)),
             label=levels(plotDat$markers),
             gp=gpar(fontsize=7, lineheight=0.75))

  seekViewport("heat2")
  grid.xaxis(seq(0.5, ncol(dataHeat2) - 0.5, length.out=ncol(dataHeat2)),
             label=levels(plotDat$variable),
             gp=gpar(fontsize=7, lineheight=0.75), edits=gEdit(gPath="labels", rot=0))
  grid.yaxis(seq(0.5, nrow(dataHeat2) - 0.5, length.out=nrow(dataHeat2)),
             label=levels(plotDat2$samples),
             gp=gpar(fontsize=7, lineheight=0.75))
  grid.draw(grheat2)
  if(!missing(group)){
  seekViewport("heat3")
  grid.draw(grheat3)
  }

  if(legend == TRUE){
    seekViewport("legend1")
    grid.draw(grlegend1)

    seekViewport("legend2")
    grid.draw(grlegend2)

    seekViewport("legend3")
    grid.draw(grlegend3)
  }
}

