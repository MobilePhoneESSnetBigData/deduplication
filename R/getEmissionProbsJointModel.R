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
getEmissionProbsJointModel <- function(emissionProbs) {
  
  emissionProbs <- cbind(emissionProbs, 'NA' = rep(1, nrow(emissionProbs)))
  jointEmissionProbs <- NULL
  for(j in 1:ncol(emissionProbs)){
    A <- emissionProbs[, j] * emissionProbs
    colnames(A) <- paste0(colnames(emissionProbs)[j], "-", colnames(emissionProbs))
    jointEmissionProbs <- cbind(jointEmissionProbs, A)
  }
  colToRem <- ncol(jointEmissionProbs)
  jointEmissionProbs <- jointEmissionProbs[, -colToRem]
  
  return(jointEmissionProbs)

}
