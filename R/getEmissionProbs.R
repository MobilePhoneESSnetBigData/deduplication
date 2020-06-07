#' @title Builds the emission probabilities for the HMM used to estimate the prosterior location probabilitities.
#' 
#' 
#' @description Builds the emission probabilities needed for the HMM used to estimate the posterior location probabilitities. 
#' In case of using simulated data, these probabilities are build using the signal strength or signal quality outputed
#' by the simulation software for each tile in the grid.
#' 
#' @return Returns a matrix with the emmission probabilitiews for the HMM. The number of rows equals the numberof
#' tiles in the grid and the number of columns equals the number of antennas.
#' 
#' 
#' @import data.table
#' @import stringr
#' @include tileEquivalence.R
#' 
#' @export
getEmissionProbs <- function(nrows, ncols, signalFileName, sigMin, handoverType = 'strength', simulatedData = TRUE) {
  if(simulatedData) {
    if(!file.exists(signalFileName))
      stop("The file with signal values doesn't exist")
    
    tileEquiv.dt <- data.table(tileEquivalence(nrows, ncols))
    
    
    RSS <- fread(signalFileName, sep = ",", header = TRUE, stringsAsFactors = FALSE)
    nTiles1 <- dim(RSS)[2] - 1
    nTiles2 <- nrows * ncols
    if (nTiles1 != nTiles2) {
      stop("Number of rows and columns provided inconsistent with the signal file")
    }
    
    setnames(RSS, c('antennaID', 0:(nTiles1 - 1)))
    RSS <- melt(RSS, id.vars = 'antennaID', variable.name = 'tile', variable.factor = FALSE, value.name = 'RSS')
    RSS[ , RSS := ifelse(RSS < sigMin, NA, RSS)]
    if (handoverType == 'strength') {
      # The radio wave model is expressed in log10. It seems natural to recover the original scale
      RSS <- RSS[, eventLoc := 10**RSS / sum(10**RSS, na.rm = TRUE), by = 'tile']
      
    }  
    # Make eventLoc=0 if the tile is out the coverage area
    RSS <- RSS[is.na(eventLoc), eventLoc := 0]
    RSS[ , tile := as.numeric(tile)]
    
    RSS <- RSS[tileEquiv.dt, on = 'tile'][  , tile := NULL]
    
    RSS <- dcast(RSS, rasterCell ~ antennaID, value.var = 'eventLoc')[, rasterCell := NULL]
    
    emissionProbs <- Matrix(data=as.matrix(RSS))
    remove(RSS)
    dimnames(emissionProbs)[[1]] <- as.character(1:dim(emissionProbs)[1])
    
    return (emissionProbs)
  }
  else {
    cat("Can't read real mobile network signal file yet!")
    return (NULL)
  }
}


