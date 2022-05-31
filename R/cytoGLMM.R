#' Generalized linear mixed model on cfList
#'
#'
#' @description Wrapper function to perform a generalized linear mixed model on 
#' each cluster in a univariate manner. 
#' 
#' @param cfList a cfList object
#' @param formula formula defined as in code{\link[lme4]{glmer}}
#' 
#' 
#' 
#' 
#' 
#' 
#' 
cytoGLMM <- function(cfList, formula){
  
  counts <- cellCounts(cfList, F, F)@counts
  modelData <- cfList@samples
  modelData$weights <- rowSums(counts)
  
  for(i in seq_len(ncol(counts))){
    
    modelData$y <- counts[,i]/modelData$weights
    glm1 <- glmer(formula,
                  data=modelData,
                  family="binomial",
                  weights=weights)
    
    # collect results
    if(i == 1){
      s <- summary(glm1)
      sNames <- dimnames(s$coefficients)[[1]]
      est <- s$coefficients[sNames, "Estimate"]
      pval <- s$coefficients[sNames[-1], "Pr(>|z|)"]
    } else {
      s <- summary(glm1)
      sNames <- dimnames(s$coefficients)[[1]]
      est <- rbind(est, s$coefficients[sNames, "Estimate"])
      pval <- rbind(pval, s$coefficients[sNames[-1], "Pr(>|z|)"])
    }
    
  }
  
  # Adjust pval
  pval_adj <- matrix(p.adjust(pval, method="fdr"), ncol=length(sNames[-1]))
  results <- data.frame(clusterID = colnames(counts),
                        est, 
                        pval,
                        pval_adj)
  rownames(results) <- NULL
  colnames(results) <- c("clusterID", 
                         paste0("est_", sNames),
                         paste0("pval_", sNames[-1]),
                         paste0("pval_", sNames[-1]))
  
  return(results)
}