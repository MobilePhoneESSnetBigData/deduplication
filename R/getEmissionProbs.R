#' @title Builds the emission probabilities for the HMM used to estimate the posterior location probabilitities.
#'
#'
#' @description Builds the emission probabilities needed for the HMM used to estimate the posterior location
#' probabilitities.
#' In case of using simulated data, these probabilities are build using the signal strength or signal quality outputed
#' by the simulation software for each tile in the grid.
#'
#' @return Returns a Matrix object with the emmission probabilities for the HMM. The number of rows equals the
#' number of tiles in the grid and the number of columns equals the number of antennas. An element (i,j) of
#' this matrix coresponds to the probability of a device being in tile i to be connected to antena j. The row
#' names of the matrix are the tile indices and the column names are the antenna IDs.
#'
#'@param nrow the number of rows in the grid. It can be obtained by calling \code{readGridParams}.
#'
#'@param ncol the number of columns in the grid. It can be obtained by calling \code{readGridParams}.
#'
#'@param signalFileName The name of the .csv file that contains the signal strength/quality for each tile in the grid.
#'This file is one of the outputs of the data simulator. The data is organized as a matrix with the number of rows
#'equals to the number of antennas and the the following columns: \code{Antenna ID, Tile 0, Tile 1, ... Tile (N-1)}.
#'On the first column there are the antenna IDs and on the rest of the columns the coresponding signal 
#'strength/quality.
#'
#'
#'@param sigMin The minimum value of the signal strength/quality that allow a connection between a device and an
#'antenna.
#'
#'@param handoverType The handover mechanism used by the mobile network. It could have two values:'strenght' or
#''quality'
#'
#'@param simulatedData If TRUE, the input data provided to this function come from tha simulator otherwise the data 
#'come from a real mobile network.
#'
#' @import data.table
#' @import stringr
#' @import Matrix
#' @import XML
#' @include tileEquivalence.R
#'
#' @export
getEmissionProbs <-
  function(nrows,
           ncols,
           signalFileName,
           sigMin,
           handoverType = 'strength',
           simulatedData = TRUE,
           emissionModel = NULL,
           antennaFileName = NULL) {
    
    if (simulatedData == TRUE) {
      if (!file.exists(signalFileName))
        stop(paste0(signalFileName, " doesn't exist!"))
      
      if( (is.null(emissionModel)  && !is.null(antennaFileName) ) || ((!is.null(emissionModel)  && is.null(antennaFileName) )) )
        stop("Either both emissionModel and antennaParam are not null or they are both null")
      
      if(handoverType == 'strength' && emissionModel == 'RSS') {
        emissionModel = NULL
        antennaFileName = NULL
      }
      if(handoverType == 'qaulity' && emissionModel == 'SDM') {
        emissionModel = NULL
        antennaFileName = NULL
      }

      tileEquiv.dt <- data.table(tileEquivalence(nrows, ncols))

      RSS <-
        fread(
          signalFileName,
          sep = ",",
          header = TRUE,
          stringsAsFactors = FALSE
        )
      antennasIDs<-RSS[,1]
      nTiles1 <- dim(RSS)[2] - 1
      nTiles2 <- nrows * ncols
      if (nTiles1 != nTiles2) {
        stop("Number of rows and columns provided inconsistent with the signal file")
      }
      
      setnames(RSS, c('antennaID', 0:(nTiles1 - 1)))
      RSS <-
        melt(
          RSS,
          id.vars = 'antennaID',
          variable.name = 'tile',
          variable.factor = FALSE,
          value.name = 'RSS'
        )
      
      if((is.null(emissionModel)  && is.null(antennaFileName) ) ) {
        
        RSS[, RSS := ifelse(RSS < sigMin, NA, RSS)]
        if (handoverType == 'strength') {
          RSS <-RSS[, eventLoc := 10 ** RSS / sum(10 ** RSS, na.rm = TRUE), by = 'tile']
          
        }
        else { 
          if(handoverType == 'quality') {
            RSS <- RSS[, eventLoc := RSS / sum(RSS, na.rm = TRUE), by = 'tile']
          }
          else {
            stop("handover method unsupported")
          }
        }
      } 
      else {
        antennas<-xmlToDataFrame(antennaFileName)
        antennas<-do.call(cbind, lapply(antennas, rev))
        
        antennas<-cbind(antennasIDs, antennas)
        cols<-colnames(antennas)
        cols[1]<-'antennaID'
        colnames(antennas)<-cols
        antennas<-antennas[,c(1,7:10)]
        RSS <- merge(RSS, antennas, all.x = TRUE, by = "antennaID")
        RSS[, SSteep := as.numeric(SSteep)]
        RSS[, Smid := as.numeric(Smid)]
        RSS[, Qmin := as.numeric(Qmin)]
        if(handoverType == 'strength' && emissionModel == 'SDM') {
          RSS[, SDM := (1/(1 + exp(-SSteep * (RSS - Smid))))]
          RSS[, SDM := ifelse(SDM < Qmin, NA, SDM)]
          RSS <- RSS[, eventLoc := SDM / sum(SDM, na.rm = TRUE), by = 'tile']
        }
        if(handoverType == 'quality' && emissionModel == 'RSS') {
          RSS[, RSS_actual := (1/(SSteep * log(RSS/(1-RSS)) ))]
          RSS[, RSS_actual := ifelse(RSS_actual < Smin, NA, RSS_actual)]
          RSS <- RSS[, eventLoc := RSS_actual / sum(RSS_actual, na.rm = TRUE), by = 'tile']
        }
      }
      
      
      # Make eventLoc=0 if the tile is out the coverage area
      RSS <- RSS[is.na(eventLoc), eventLoc := 0]
      RSS[, tile := as.numeric(tile)]
      
      RSS <- RSS[tileEquiv.dt, on = 'tile'][, tile := NULL]
      
      RSS <-
        dcast(RSS, rasterCell ~ antennaID, value.var = 'eventLoc')[, rasterCell := NULL]
      
      emissionProbs <- Matrix(data = as.matrix(RSS))
      remove(RSS)
      
      dimnames(emissionProbs)[[1]] <-
        as.character(1:dim(emissionProbs)[1])
      
      return (emissionProbs)
    }
    else {
      cat("Can't read real mobile network signal file yet!")
      return (NULL)
    }
  }
