#' @title Builds the emmissions probabilities for the HMM used to estimate the prosterior location probabilitities.
#' 
#' 
#' @description Builds the emmissions probabilities for the HMM used to estimate the posterior location probabilitities. 
#' In case of using simulated data, these probabilities are build using the signal strength or signal quality outputed
#' by the simulation software for each tile in the grid.
#' 
#' @return Returns a matrix with the emmission probabilitiews for the HMM. The 
#' 
#' 
#' @import data.table
#' @include tileEquivalence.R
#' 
#' @export
getEmmissionProbs <- function(nrows, ncols, signalFileName, handoverType = 'strength') {
  
  if(!file.exists(signalFileName))
    stop("The file with signal values doesn't exist")
  
  tileEquiv.dt <- data.table(tileEquivalence(nrows, ncols))


  RSS_Sim.dt <- fread(signalFileName, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  nTiles1 <- dim(RSS_Sim.dt)[2] - 1
  nTiles2 <- nrows * ncols
  if (nTiles1 != nTiles2) {
    stop("Number of rows and cols provided inconsistent with the signal file")
  }
    
  setnames(RSS_Sim.dt, c('antennaID', 0:(nTiles - 1)))
  RSS_Sim.dt <- melt(RSS_Sim.dt, id.vars = 'antennaID', variable.name = 'tile', variable.factor = FALSE, value.name = 'RSS')
  RSS_Sim.dt <- RSS_Sim.dt[, antennaID := str_pad(antennaID, max(nchar(antennaID)), pad="0")]
  
  if (handoverType == 'strength') {
    # The radio wave model is expressed in log10. It seems natural to recover the original scale
    RSS_Sim.dt <- RSS_Sim.dt[, eventLoc := 10**RSS / sum(10**RSS, na.rm = TRUE), by = 'tile']
    
  }  
  # Make eventLoc=0 if the tile is out the coverage area
  RSS_Sim.dt <- RSS_Sim.dt[is.na(eventLoc), eventLoc := 0]
  RSS_Sim.dt[ , tile := as.numeric(tile)]
  
  RSS_Sim.dt <- RSS_Sim.dt[tileEquiv.dt, on = 'tile'][  , tile := NULL]

  tempDT.dt <- dcast(RSS_Sim.dt, rasterCell ~ antennaID, value.var = 'eventLoc')[, rasterCell := NULL]
  remove(RSS_Sim.dt)
  eventLoc_rasterCell.matrix <- as.matrix(tempDT.dt)
  dimnames(eventLoc_rasterCell.matrix)[[1]] <- as.character(1:dim(eventLoc_rasterCell.matrix)[1])
  
  return (eventLoc_rasterCell.matrix)

}
