#' Scale the expression values of a cfList object
#'
#' @description This function scales the expression values.
#'
#' @param cfList a cfList object.
#' @param scale: 
#' * clusterMax: for each marker, there is a cluster that has an
#'   expression value that is at maximum. So the expression values are scaled
#'   such the maximal value of a cluster is top limit. The scaling does not
#'   change the zero: it is just a multiplicative coefficient.
#' @param expr.limits is a vector of to numerical values, the minimum and the
#'   maximum of the expression values.
#'
#' @return The function returns an object of class
#'   \code{\link[cytofast]{cfList}}.
#'
#' @note
#'
#' @keywords FCS, expression
#'
#' @examples
#'
#' @export
cytoExprScale <- function(cfList, scale = "clusterMax", expr.limits = c(0, 6)) {
  
  scale = match.arg(scale)
  
  # Extract expression matrix
  X <- cfList@expr[, !colnames(cfList@expr) %in% c("clusterID", "sampleID")]
  clusterID <- factor(cfList@expr$clusterID)
  sampleID <- factor(cfList@expr$sampleID)
  # Compute cluster expression matrix
  dataMed <- data.frame(matrix(0, ncol = ncol(X), nrow = length(levels(clusterID))))
  for (i in seq_len(ncol(X))) {
    dataMed[, i] <- tapply(X[, i], clusterID, median)
  }
  colnames(dataMed) <- colnames(X)
  rownames(dataMed) <- levels(clusterID)
  # Compute min and max per channel
  channel.max <- apply(dataMed, 2, max)
  channel.min <- apply(dataMed, 2, min)
  # Avoid zero
  channel.max[channel.max == 0] <- 1
  # Do scaling
  if(scale == "clusterMax") {
    # Scale channel to max across cluster
    cfList@expr[,-(1:2)] <- sweep(cfList@expr[,-(1:2)], 2, channel.max, "/")
    cfList@expr[,-(1:2)] <- cfList@expr[,-(1:2)] * expr.limits[2]
  }  
  return(cfList)
}
