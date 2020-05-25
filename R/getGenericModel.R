#' @title Buildsw the generic HMM model.
#' 
#' @description Buildsw the generic HMM model using the emmission probabilities which tare the event locations
#' computed using the mobile network parameters.
#' 
#' @param nrows Number of rows in the grid.
#' 
#' @param ncols Number of columns in the grid.
#' 
#' @param emmission probs A matrix with the event location probabilities. The number of rows equals the numberof
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
getGenericModel <- function(nrows, ncols, emmissionProbs, initSteady = TRUE) {
  
  model <- HMMrectangle(nrows, ncols)
  emissions(model) <- emmisionProbs       # event location for each antenna
  model <- initparams(model)              # initial transition prob.
  model <- minparams(model)               # reduction of parameters based on restrictions
  if(initSteady)
    model <- initsteady(model)             
  
  return (model)
}