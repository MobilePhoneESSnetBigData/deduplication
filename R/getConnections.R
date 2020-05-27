#' @title Builds a matrix object containing the IDs of the antennas to which devices are connected
#' 
#' @description Builds a matrix object containing the IDs of the antennas to which devices are connected. 
#' The number of rows equals the number of devices and the number of columns equals the number of time instants 
#' when the network events were recorded. 
#' An element \code{[i,j]} in the returned matrix equals the ID of the antenna where the mobile device with index \code{i} 
#' in the input vector \param{devices} is conntected at the time instant with index \code{j} in the input vector \param{times}.
#' 
#' @param devices A vector with the devices IDs
#' 
#' @param events.dt A data.table object returned by \link{readEvents} function
#' 
#' @return A matrix object with the antenna IDs where devices are connected for every time instants in the event file. 
#' If a device is not connected to any antenna at a time instant the corresponding element in the matrix will have the value NA.
#' 
#' @import data.table
#' 
#' @export
getConnetions <- function(events.dt) {
  
  times <- unique(events.dt[,1])
  times <- sorted(times)
  devices <- unique(events.dt[,4])
  devices <- sorted(devices)
  
  n1 <- length(times)
  n2 <- length(devices)
  
  connections.dt <- matrix(ncol = n1, nrow = n2)

  evv <- matrix(ncol = 2, nrow = n1)
  evv[,1] <- times
  colnames(evv)<-c('time', 'antennaID')
  
  for(i in 1:n2){
    evv <- merge(evv, events.dt[deviceID == devices[i],][,c(1,4)], by='time', all.x = TRUE)[,c(1,3)]
    connections.dt[i,] <- t(evv[,2])
  }
  return (connections.dt)
}