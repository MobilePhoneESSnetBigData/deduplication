#' Example of using deduplication package - using trajectory approach
#' 
#' #This is just an example on how to compute duplicity probabilities using simulated data. All the files used in this
#' #example are supposed to be produced using the simulation software. The "simulation.xml" file is an exception and it
#' #is an input file for the simulation software. The files used in this example are provided with the deduplication
#' #package.
#'
#' 
#' # set the folder where the necessary input files are stored
#' 
#' path_root      <- 'extdata'
#' 
#' 
#' # 1. Read grid parameters
#'  
#' gridParams <-readGridParams(system.file(path_root, 'grid.csv', package = 'deduplication'))
#' 
#' centrs <- buildCentroids(gridParams$ncol, gridParams$nrow, gridParams$tileX, gridParams$tileY)
#' 
#' events <- readEvents(system.file(path_root, 'AntennaInfo_MNO_MNO1.csv', package = 'deduplication'))
#'
#' # 3. Get a list of detected devices
#'  
#' devices <- getDeviceIDs(events)
#'
#' postLoc <- list()
#' centerOfProbs <- list() 
#' 
#' for( i in 1:length(devices)) {
#'     postLoc[[i]] <- readPostLocProb(path_root, devices[i])
#'     centerOfProbs[[i]] <- centerOfProbabilities(centrs, postLoc[[i]])
#' }
#' 
#' 
example3 <- function() {}