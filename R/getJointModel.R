#' @title Builds the joint HMM model.
#' 
#' @description Builds the joint HMM model using the emmission probabilities which are the event locations
#' computed using the mobile network parameters.
#' 
#' @param nrows Number of rows in the grid.
#' 
#' @param ncols Number of columns in the grid.
#' 
#' @param jointEmissionProbs A matrix with the joint event location probabilities. The number of rows equals the number of
#' tiles in the grid and the number of columns equals the number of antennas plus one.  
#'
#' @param initSteady If true the initial a-priori distribution is set to the steady state of the transition matrix.
#' 
#' 
#' @return Returns an HMM model with the initial a-priori distribution set to the steady state of the transition matrix.
#' 
#' 
#' @import destim
#'
#' @export
getJointModel <- function(nrows, ncols, jointEmissionProbs, initSteady = TRUE) {
  
  modeljoin <- HMMrectangle(nrows, ncols)
  emissions(modeljoin) <- jointEmissionProbs 
  modeljoin <- initparams(modeljoin)        
  modeljoin <- minparams(modeljoin)         
  
  checkE <- apply(jointEmissionProbs, 2, sum)
  
  if (initSteady == TRUE)
    modeljoin <-initsteady(modeljoin)
  
  return (modeljoin)
}