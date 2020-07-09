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
#' @import Matrix
#' @export
readPostLocProb <-function(path, deviceID) {
  file <- system.file(path, paste0("postLocDevice_", as.character(deviceID), ".csv"), package = 'deduplication')
  if(!file.exists(file))
    stop (paste0('file with posterior location probabilities files does not exist ', file))

  postLoc <- fread(file, sep = ',',stringsAsFactors = FALSE,header = FALSE)
  
  return (Matrix(as.matrix(postLoc)))
  
}