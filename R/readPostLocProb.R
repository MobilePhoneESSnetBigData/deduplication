#' @title Reads a file with the posterior location probabilities.
#'
#' @description Reads a .csv file with the posterior location probabilities. Each row of the file
#'   corresponds to a tile and each column corresponds to a time instant.
#'
#' @param path The path to the location where the posterior location probabilities are stored.
#'
#' @param deviceID The device ID for which the posterior location probabilities are read.
#'
#' @return a data.table object with the posterior location probabilities for the device with Id
#'   equals to deviceID. A row corresponds to a tile and a column corresponds to a time instant.
#'
#' @import data.table
#' @export
readPostLocProb <-function(path, deviceID) {
  if(!file.exists(path))
    stop ('directory with posterior location probabilities files does not exist')
  
  filename <- paste0(path, "postLocDevice_", as.character(deviceID), ".csv")
  
  if(!file.exists(filename))
    stop (paste0('posterior location probabilities file for device ', deviceID,  ' does not exist'))
  
  postLoc <- fread(filename, sep = ',',stringsAsFactors = FALSE,header = FALSE)
  
  return (postLoc)
  
}