#' @title Builds the generic HMM model.
#'
#' @description Builds the generic HMM model using the emission probabilities which are the event locations
#' computed using the mobile network parameters.
#'
#' @param nrows Number of rows in the grid.
#'
#' @param ncols Number of columns in the grid.
#'
#' @param emissionProbs A matrix with the event location probabilities. The number of rows equals the number of
#' tiles in the grid and the number of columns equals the number of antennas plus one.
#'
#' @param initSteady If TRUE the initial a-priori distribution is set to the steady state of the transition matrix.
#'
#' @return Returns an HMM model with the initial a-priori distribution set to the steady state of the transition matrix.
#'
#' @import destim
#'
#' @export
getGenericModel <-  function(nrows, ncols, emissionProbs, initSteady = TRUE) {
    model <- HMMrectangle(nrows, ncols)
    emissions(model) <- emissionProbs
    model <- initparams(model)
    model <- minparams(model)
    
    if (initSteady)
      model <- initsteady(model)
    
    return (model)
  }