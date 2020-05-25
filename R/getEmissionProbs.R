#' @title Builds the emission probabilities for the HMM used to estimate the prosterior location probabilitities.
#' 
#' 
#' @description Builds the emissions probabilities needed for the HMM used to estimate the posterior location probabilitities. 
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
getEmmissionProbs <- function(nrows, ncols, signalFileName, handoverType = 'strength') {
  
  if(!file.exists(signalFileName))
    stop("The file with signal values doesn't exist")
  
  tileEquiv.dt <- data.table(tileEquivalence(nrows, ncols))


  RSS.dt <- fread(signalFileName, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  nTiles1 <- dim(RSS.dt)[2] - 1
  nTiles2 <- nrows * ncols
  if (nTiles1 != nTiles2) {
    stop("Number of rows and columns provided inconsistent with the signal file")
  }
    
  setnames(RSS.dt, c('antennaID', 0:(nTiles - 1)))
  RSS.dt <- melt(RSS.dt, id.vars = 'antennaID', variable.name = 'tile', variable.factor = FALSE, value.name = 'RSS')
  RSS.dt <- RSS.dt[, antennaID := str_pad(antennaID, max(nchar(antennaID)), pad="0")]
  
  if (handoverType == 'strength') {
    # The radio wave model is expressed in log10. It seems natural to recover the original scale
    RSS.dt <- RSS.dt[, eventLoc := 10**RSS / sum(10**RSS, na.rm = TRUE), by = 'tile']
    
  }  
  # Make eventLoc=0 if the tile is out the coverage area
  RSS.dt <- RSS.dt[is.na(eventLoc), eventLoc := 0]
  RSS.dt[ , tile := as.numeric(tile)]
  
  RSS.dt <- RSS.dt[tileEquiv.dt, on = 'tile'][  , tile := NULL]

  RSS.dt <- dcast(RSS.dt, rasterCell ~ antennaID, value.var = 'eventLoc')[, rasterCell := NULL]

  emmissionProbs.matrix <- as.matrix(RSS.dt)
  dimnames(emmissionProbs.matrix)[[1]] <- as.character(1:dim(emmissionProbs.matrix)[1])
  
  return (emmissionProbs.matrix)

}


