#' @title Transforms the tiles indices from the notation used by the simulation software to the one used by the raster package
#' 
#'  
#' @description Transform the tiles indices from the notation used by the simulation software to the one used by the raster package. 
#' The simulation software uses a notation where the tile with index 0 is the bottom left tile while the raster package uses another
#' way to number the tiles, tiles being numbered starting with 1 for the upper left tile.
#' 
#' @param nrow Number of rows in the grid overpalling the area of interest
#' 
#' @param ncol Number of columns in the grid overpalling the area of interest
#' 
#' @return Returns a data.frame object with two columns: on the first column are the tile indices according to the raster package 
#'  numbering and on the second column are equivalent tile indices accordin to the simulation oftware numbering
#' 
#'
#' @export
tileEquivalence <- function(nrow, ncol){
  
  nCells <- nrow * ncol
  
  tileID_raster <- as.vector(matrix(1:nCells, ncol = ncol, byrow = TRUE))
  tileID_simulator <- as.vector(matrix(0:(nCells - 1), ncol = ncol, byrow = TRUE)[nrow:1, ])

  tileCorresp <- cbind(rasterCellID = tileID_raster, tileID = tileID_simulator)
  tileCorresp <- tileCorresp[order(tileCorresp[, 'rasterCellID']), ]
  return(tileCorresp)
}
