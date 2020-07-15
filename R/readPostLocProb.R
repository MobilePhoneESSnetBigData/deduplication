#' @title Reads a file with the posterior location probabilities.
#'
#' @description Reads a .csv file with the posterior location probabilities. Each row of the file corresponds to a tile
#'   and each column corresponds to a time instant.
#'
#' @param path The path to the location where the posterior location probabilities are stored. The file with the
#'   location probabilities should have the name \code{postLocDevice_ID.csv} where \code{ID} is replaced with the device
#'   ID.
#'
#' @param deviceID The device ID for which the posterior location probabilities are read.
#'
#' @return A Matrix object with the posterior location probabilities for the device with ID equals to deviceID. A
#'   row corresponds to a tile and a column corresponds to a time instant.
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