#' @title Reads a file with the posterior location probabilities.
#'
#' @description Reads a .csv file with the posterior location probabilities. There are 3 values on each row of the file:
#'   \code{time, tile, prob}. Only the nonzero probabilities are given in this file.
#'
#' @param path The path to the location where the posterior location probabilities are stored. The file with the
#'   location probabilities should have the name \code{postLocDevice_ID.dt.csv} where \code{ID} is replaced with the
#'   device ID.
#'   
#' @param prefixName The file name prefix. The whole file name is composed by a concatenation of \code{prefixName},
#'   \code{_},  \code{deviceID} and the extension \code{.dt.csv}.
#'
#' @param deviceID The device ID for which the posterior location probabilities are read.
#'
#' @return A data.table object with the posterior location probabilities for the device with ID equals to deviceID. It
#'   has the following columns: \code{time, tile, prob}. Only the nonzero probabilities are returned.
#'
#' @import data.table
#' @export
readPostLocProb <- function(path, prefixName, deviceID) {
  file <- paste0(path, "/", prefixName, "_", as.character(deviceID), ".dt.csv")
  if (!file.exists(file))
    stop (paste0('file with posterior location probabilities files does not exist ',file))
  
  postLoc <-
    fread(file,
          sep = ',',
          stringsAsFactors = FALSE,
          header = TRUE)
  
  return (postLoc)
  
}