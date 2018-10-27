#' Draw a dendogram with ggplot
#'
#' @description
#' Function to draw only a dendrogram with ggplot, the tree can be easily alligned to other graphs (e.g. heatmaps).
#'
#' @param hclust an object of class \code{\link[stats]{hclust}}.
#' 
#' @return None
#'
#' @keywords dendrogram heatmap tree
#'
#' @examples 
#' hc <- hclust(dist(data.frame(x1 = rnorm(10), x2 = rnorm(10), x3 = rnorm(10))))
#' drawTree(hc)
#'
#' @import ggplot2
#'
#' @export

drawTree <- function(hclust){

  # private variables
  newCoords <- data.frame(xCoord=numeric(), yCoord=numeric())
  tree <- data.frame(hclust$merge, height=hclust$height)
  order <- hclust$order

  # set-up ggplot
  dummy <- data.frame(x=factor(seq_len(max(order))), y=seq(0, max(hclust$height), length.out=max(order)))
  g1 <- ggplot(dummy) + geom_blank(aes_string(x="x", y="y")) + theme_void()

  for(i in seq_len(nrow(tree))){

    # Set coordinates
    xCoord <- match(abs(tree[i, seq_len(2)]), order)
    yCoordLeft <- c(tree$height[i], 0)
    yCoordRight <- c(tree$height[i], 0)

    # check if one of the coordinates was postive
    if(tree[i, 1] > 0){
      xCoord[1] <- newCoords[tree[i,1], 1]
      yCoordLeft[2] <- newCoords[tree[i,1], 2]
    }
    if(tree[i, 2] > 0){
      xCoord[2] <- newCoords[tree[i,2], 1]
      yCoordRight[2] <- newCoords[tree[i,2], 2]
    }

    # horizontal lines
    g1 <- g1 + geom_segment(x=xCoord[1], xend=xCoord[2], y=yCoordLeft[1], yend=yCoordRight[1])

    # vertical lines
    g1 <- g1 + geom_segment(x=xCoord[1], xend=xCoord[1], y=yCoordLeft[1], yend=yCoordLeft[2]) +
               geom_segment(x=xCoord[2], xend=xCoord[2], y=yCoordRight[1], yend=yCoordRight[2])

    # save new coords for further building
    newCoords <- rbind(newCoords, c(mean(xCoord), yCoordLeft[1]))

  }
  return(g1)
}









