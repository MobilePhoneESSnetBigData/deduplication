#' @title Builds the emission probabilities for the joint HMM
#' 
#' 
#' @description Builds the emissions probabilities needed for the joint HMM used to estimate the posterior location probabilitities. 
#' In case of using simulated data, these probabilities are build using the signal strength or signal quality outputed
#' by the simulation software for each tile in the grid.
#' 
#' @return Returns a matrix with the joint emission probabilities for the HMM. The number of rows equals the numberof
#' tiles in the grid and the number of columns equals the number of antennas plus one.
#' 
#' 
#' @import data.table
#' @include tileEquivalence.R
#' 
#' @export
getEmmissionProbsJointModel <- function(emmisionProbs) {
  eventLoc_rasterCell.matrix <- cbind(eventLoc_rasterCell.matrix, '00' = rep(1, nrow(eventLoc_rasterCell.matrix)))
  new_eLoc_rCell.mt <- NULL
  for(j in 1:ncol(eventLoc_rasterCell.matrix)){
    cat(paste0(j, ", "))
    A <- eventLoc_rasterCell.matrix[, j] * eventLoc_rasterCell.matrix
    colnames(A) <- paste0(colnames(eventLoc_rasterCell.matrix)[j], "-", colnames(eventLoc_rasterCell.matrix))
    new_eLoc_rCell.mt <- cbind(new_eLoc_rCell.mt, A)
  }
  colToRem <- which(colnames(new_eLoc_rCell.mt) == "00-00")
  new_eLoc_rCell.mt <- new_eLoc_rCell.mt[, -colToRem]
  
  return(new_eLoc_rCell.mt)

}
