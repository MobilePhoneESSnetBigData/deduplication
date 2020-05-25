#' @title Builds the emission probabilities for the joint HMM.
#' 
#' 
#' @description Builds the emissions probabilities needed for the joint HMM used to estimate the posterior location probabilitities. 
#' In case of using simulated data, these probabilities are build using the signal strength or signal quality outputed
#' by the simulation software for each tile in the grid.
#' 
#' @return Returns a matrix with the joint emission probabilities for the HMM. The number of rows equals the number of
#' tiles in the grid and the number of columns equals the number of antennas plus one.
#' 
#' 
#' @import data.table
#' @include tileEquivalence.R
#' 
#' @export
getEmissionProbsJointModel <- function(emisionProbs) {
  
  emisionProbs <- cbind(emisionProbs, '00' = rep(1, nrow(emisionProbs)))
  jointEmisionProbs <- NULL
  for(j in 1:ncol(emisionProbs)){
    cat(paste0(j, ", "))
    A <- emisionProbs[, j] * emisionProbs
    colnames(A) <- paste0(colnames(emisionProbs)[j], "-", colnames(emisionProbs))
    jointEmisionProbs <- cbind(jointEmisionProbs, A)
  }
  colToRem <- which(colnames(jointEmisionProbs) == "00-00")
  jointEmisionProbs <- jointEmisionProbs[, -colToRem]
  
  return(jointEmisionProbs)

}
