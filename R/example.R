library(data.table)
library(stringr)
library(rgeos)

path_root      <- '/home/bogdan/r-projects/deduplication-dev/csv_outputSimulator'
path_root2     <- '/home/bogdan/r-projects/deduplication-dev/xml_inputSimulator'


#0. Read simulation params
simParams <-readSimulationParams(file.path(path_root2, 'simulation.xml'))

#1. Read grid parameters
gridParams <-readGridParams(file.path(path_root, 'grid.csv'))

#2.Read network events
events <- readEvents(file.path(path_root, 'AntennaInfo_MNO_MNO1.csv'))

#3. Get a list of detected devices 
devices <- getDeviceIDs(events)

#4. Read antennas file and build a matrix of neighboring antennas
coverarea <- readCells(file.path(path_root, 'AntennaCells_MNO1.csv'))
antennaNeigh <- antennaNeighbours(coverarea)

#5. Get connections for each device
connections <- getConnections(events)

#6. Emission probabilities are computed from the signal strength/quality file
emissionProbs <- getEmissionProbs(gridParams$nrow, gridParams$ncol, file.path(path_root, 'SignalMeasure_MNO1.csv'))

#6. Build emission joint probabilities
jointEmissionProbs <- getEmissionProbsJointModel(emissionProbs)

#7. Build the generic model
model <- getGenericModel(gridParams$nrow, gridParams$ncol, emissionProbs)

#8. Build the joint model
modelJ <- getJointModel(gridParams$nrow, gridParams$ncol, jointEmissionProbs)

#9. Build a matrix of pairs of devices to compute duplicity probability
pairs4dup<-computePairs(connections, length(devices), antennaNeigh, P1, limit = 0.05 )

#10.Fit models
system.time(ll <- fitModels(length(devices), model,connections))


#11. Compute duplicity probabilities
P1 <- aprioriDuplicityProb(simParams$prob_sec_mobile_phone, length(devices))
out_duplicity <- compute_duplicity_Bayesian(method = "pairs", deviceIDs,pairs4duplicity = pairs4duplicity, P1 = P1, modeljoin = modelJ,
                                            logLik = ll, init = TRUE)
