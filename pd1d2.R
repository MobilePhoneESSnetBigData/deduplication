
path_root <- 'extdata'
gridParams <-readGridParams(system.file(path_root, 'grid.csv', package = 'deduplication'))
centrs <- buildCentroids(gridParams$ncol, gridParams$nrow, gridParams$tileX, gridParams$tileY)
events <- readEvents(system.file(path_root, 'AntennaInfo_MNO_MNO1.csv', package = 'deduplication'))
simParams <-readSimulationParams(system.file(path_root, 'simulation.xml', package = 'deduplication'))
connections <- getConnections(events)
coverarea <- readCells(system.file(path_root, 'AntennaCells_MNO1.csv', package = 'deduplication'))
antennaNeigh <- antennaNeighbours(coverarea)
pairs4dup<-computePairs(connections, length(devices), oneToOne = FALSE, P1=P1, limit = 0.05, antennaNeighbors = antennaNeigh)


devices <- sort(as.numeric(getDeviceIDs(events)))
postLoc <- list() 
centerOfProbs <- list()
dr<-list()
mdelta<-list(length=90)

index_i<-pairs4dup[,1]
index_j<-pairs4dup[,2]

for( i in 1:length(devices)) {
  postLoc[[i]] <- readPostLocProb(path_root, devices[i]) 
  centerOfProbs[[i]] <- centerOfProbabilities(centrs, postLoc[[i]])
  dr[[i]] <- dispersionRadius(centrs,postLoc[[i]], centerOfProbs[[i]])
}

P1 <- aprioriDuplicityProb(simParams$prob_sec_mobile_phone, length(devices))
P2 <- 1 - P1
alpha<-P1/P2
n<-length(devices)
T<-nrow(unique(events[,1]))
pd1d2<-matrix(0L, nrow = n, ncol = n )

t1 <- Sys.time()
for(i in 1:n) {
  cat(paste0("i:", i, ', '))
  if(i<n) {
    for(j in (i+1):n) {
      cat(paste0(j, ","))
      s1<-0
      mm<-max(dr[[i]], dr[[j]])
      for(t in 1:T) {
        mdelta[[t]]<-buildDeltaProb5(centrs, postLoc[[i]][,t], postLoc[[j]][,t])
        s1<-s1+(abs(modeDelta(mdelta[[t]][[1]]))<mm & abs(modeDelta(mdelta[[t]][[2]]))<mm)
      }
      tmp <- s1/T
      pd1d2[i,j] <- 1- 1/(1+alpha*tmp/(1-tmp))
      pd1d2[j,i] <- pd1d2[i,j]
      if( pd1d2[i,j]>0.5)
        print(paste0(i, ":", j))
    }
  }
}
t2 <- Sys.time()
total_time <-  difftime(t2, t1, units = "mins")
total_time

t3 <- Sys.time()
# for(i in 1:nrow(pairs4dup)) {
#   print(paste0("i:", i))
#   index_i <-pairs4dup[i,1][[1]]
#   index_j <-pairs4dup[i,2][[1]]
#   s1<-0
#   mm<-max(dr[[index_i]], dr[[index_j]])
#   for(t in 1:T) {
#     mdelta[[t]]<-buildDeltaProb5(centrs, postLoc[[index_i]][,t], postLoc[[index_j]][,t])
#     s1<-s1+(abs(modeDelta(mdelta[[t]][[1]]))<mm & abs(modeDelta(mdelta[[t]][[2]]))<mm)
#   }
#   tmp <- s1/T
#   pd1d2[index_i,index_j] <- 1- 1/(1+alpha*tmp/(1-tmp))
#   pd1d2[index_j,index_i] <- pd1d2[i,j]
#   if( pd1d2[index_i,index_j]>0.5)
#     print(paste0(index_i, ":", index_j))
#   
# }
t4 <- Sys.time()
total_time2 <-  difftime(t2, t1, units = "mins")
total_time2

dup<-data.table(devices=devices, dupP=0)
for(i in 1:nrow(pd1d2)) {
  dup[i,2]<-max(pd1d2[i,])
}  


