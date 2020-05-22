

getEmmissionProbs <- function() {
  tileEquiv.dt <- data.table(tileEquivalence(ncol_grid, nrow_grid))
  setnames(tileEquiv.dt, 'tileID', 'tile')
  setnames(tileEquiv.dt, 'rasterCellID', 'rasterCell')

  RSS_Raster.dt <- copy(RSS_Sim.dt)[, tile := as.integer(tile)]
  RSS_Raster.dt <- RSS_Raster.dt[tileEquiv.dt, on = 'tile'][  , tile := NULL]
  colToRemove <- intersect(names(RSS_Raster.dt), c("RSS", "RSQ", "Smid", "SSteep"))
  set(RSS_Raster.dt, j = colToRemove, value = NULL)
  tempDT.dt <- dcast(RSS_Raster.dt, rasterCell ~ antennaID, value.var = 'eventLoc')[, rasterCell := NULL]
  eventLoc_rasterCell.matrix <- as.matrix(tempDT.dt)
  dimnames(eventLoc_rasterCell.matrix)[[1]] <- as.character(1:dim(eventLoc_rasterCell.matrix)[1])

}