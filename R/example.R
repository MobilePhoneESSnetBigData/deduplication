library(data.table)
library(stringr)


path_root      <- '/home/bogdan/r-projects/deduplication-dev/csv_outputSimulator'

#1. read grid parameters
gridParams <-readGridParams(file.path(path_root, 'grid.csv'))

#2.read events
events.dt <- readEvents(file.path(path_root, 'AntennaInfo_MNO_MNO1.csv'))

#3. get a list of devices 
devices <- getDeviceIDs(events.dt)

#4. read antennas and build a matrix of neighbours
antennasNeigh <- antennasNeighbours(file.path(path_root, 'antennas.csv'))

#5. get connections for each device
connections <- getConnections(events.dt)

emissionProbs <- getEmissionProbs(gridParams$nrow, gridParams$ncol, file.path(path_root, 'SignalMeasure_MNO1.csv'))

#6. emission joint probs
jointEmissionProbs <- getEmissionProbsJointModel(emissionProbs)

#7. build generic model
model <- getGenericModel(gridParams$nrow, gridParams$ncol, emissionProbs)

#8. build joint model
modelJ <- getJointModel(gridParams$nrow, gridParams$ncol, jointEmissionProbs)

