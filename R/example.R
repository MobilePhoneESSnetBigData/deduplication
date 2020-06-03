library(data.table)
library(stringr)
library(rgeos)

path_root      <- '/home/bogdan/r-projects/deduplication-dev/csv_outputSimulator'

#1. Read grid parameters
gridParams <-readGridParams(file.path(path_root, 'grid.csv'))

#2.Read network events
events.dt <- readEvents(file.path(path_root, 'AntennaInfo_MNO_MNO1.csv'))

#3. Get a list of detected devices 
devices <- getDeviceIDs(events.dt)

#4. Read antennas file and build a matrix of neighboring antennas
coverarea <- readCells(file.path(path_root, 'antennas.csv'))
antennasNeigh <- antennasNeighbours(coverarea)

#5. Get connections for each device
connections <- getConnections(events.dt)

#6. Emission probabilities are computed from the signal strength/quality file
emissionProbs <- getEmissionProbs(gridParams$nrow, gridParams$ncol, file.path(path_root, 'SignalMeasure_MNO1.csv'))

#6. Build emission joint probabilities
jointEmissionProbs <- getEmissionProbsJointModel(emissionProbs)

#7. Build the generic model
model <- getGenericModel(gridParams$nrow, gridParams$ncol, emissionProbs)

#8. Build the joint model
modelJ <- getJointModel(gridParams$nrow, gridParams$ncol, jointEmissionProbs)

