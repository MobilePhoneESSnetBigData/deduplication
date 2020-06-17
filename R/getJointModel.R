#' @title Builds the joint HMM model.
#'
#' @description Builds the joint HMM model using the emmission probabilities which are the event locations computed
#'   using the mobile network parameters.
#'
#' @param nrows Number of rows in the grid.
#'
#' @param ncols Number of columns in the grid.
#'
#' @param jointEmissionProbs A (sparse) matrix with the joint event location probabilities. The number of rows equals
#'   the number of tiles in the grid and the number of columns equals the number of antennas. This matrix is obtained by
#'   calling \code{getEmissionProbsJointModel}.
#'
#' @param initSteady If TRUE the initial a-priori distribution is set to the steady state of the transition matrix.
#'
#'
#' @return Returns a joint HMM model.
#'
#' @import destim
#' @export
#' 
getJointModel <-
  function(nrows,
           ncols,
           jointEmissionProbs,
           initSteady = TRUE) {
   
    modeljoin <- HMMrectangle(nrows, ncols)
    emissions(modeljoin) <- jointEmissionProbs
    modeljoin <- initparams(modeljoin)
    modeljoin <- minparams(modeljoin)
    
    if (initSteady == TRUE)
      modeljoin <- initsteady(modeljoin)
    
    return (modeljoin)
  }