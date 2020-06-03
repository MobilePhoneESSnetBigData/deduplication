library(data.table)
library(stringr)


path_root      <- '/home/bogdan/r-projects/deduplication-dev/csv_outputSimulator'

#1. read grid parameters
gridParams <-readGridParams(file.path(path_root, 'grid.csv'))

#2.read events
events.dt <- readEvents(file.path(path_root, 'AntennaInfo_MNO_MNO1.csv'))

#3. get a list of devices 
devices <- getDeviceIDs(events.dt)

#4. get connections for each device
connections <- getConnections(events.dt)

#5. emission probs
emissionProbs <- getEmissionProbs(gridParams$nrow, gridParams$ncol, file.path(path_root, 'SignalMeasure_MNO1.csv'))

#6. emmsion joint probs
jointEmissionProbs <- getEmissionProbsJointModel(emissionProbs)

#6. build generic model
model <- getGenericModel(gridParams$nrow, gridParams$ncol, emissionProbs)

#7. build joint model
modelJ <- getJointModel(gridParams$nrow, gridParams$ncol, jointEmissionProbs)

