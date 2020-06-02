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

