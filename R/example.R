# # library(data.table)
# # library(stringr)
# # library(rgeos)
# # library(parallel)
# # library(doParallel)
# # library(xml2)
# # library(Matrix)
# # library(destim)
# # 
# # 
# path_root3      <- '/home/bogdan/r-projects/deduplication-dev/outputsimulator'
# path_root4     <- '/home/bogdan/r-projects/deduplication-dev/inputSimulator'
# 
# #0. Read simulation params
# simParams <-readSimulationParams(file.path(path_root4, 'simulation.xml'))
# 
# #1. Read grid parameters
# gridParams <-readGridParams(file.path(path_root3, 'grid.csv'))
# 
# #2.Read network events
# events <- readEvents(file.path(path_root3, 'AntennaInfo_MNO_MNO1.csv'))
# 
# #3. Get a list of detected devices
# devices <- getDeviceIDs(events)
# 
# #4. Read antennas file and build a matrix of neighboring antennas
# coverarea <- readCells(file.path(path_root3, 'AntennaCells_MNO1.csv'))
# antennaNeigh <- antennaNeighbours(coverarea)
# 
# #5. Get connections for each device
# connections <- getConnections(events)
# 
# #6. Emission probabilities are computed from the signal strength/quality file
# emissionProbs <- getEmissionProbs(gridParams$nrow, gridParams$ncol, file.path(path_root3, 'SignalMeasure_MNO1.csv'), simParams$conn_threshold)
# 
# #6. Build emission joint probabilities
# jointEmissionProbs <- getEmissionProbsJointModel(emissionProbs)
# 
# #7. Build the generic model
# model <- getGenericModel(gridParams$nrow, gridParams$ncol, emissionProbs)
# 
# #8. Build the joint model
# modelJ <- getJointModel(gridParams$nrow, gridParams$ncol, jointEmissionProbs)
# 
# #9. Build a matrix of pairs of devices to compute duplicity probability
# P1 <- aprioriDuplicityProb(simParams$prob_sec_mobile_phone, length(devices))
# pairs4dup<-computePairs(connections, length(devices), antennaNeigh, P1, limit = 0.05 )
# 
# #10.Fit models
# ll <- fitModels(length(devices), model,connections)
# 
# #11. Compute duplicity probabilities
# out_duplicity <- computeDuplicityBayesian("pairs", devices, pairs4dup, modelJ, ll, P1)
