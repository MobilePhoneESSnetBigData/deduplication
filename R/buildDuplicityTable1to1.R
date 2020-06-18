#' @title  Builds a datatable with device IDs and the duplicity probability for each device.
#'
#' @description Builds a datatable with with two columns: device IDs and the duplicity probability for each device. This
#'   function is called in case of using the "1to1" method of computing the dupicity probabilities, receiving a list of
#'   matrix objects returned by each worker node with lines correponding to a subset of devices. These parts a put
#'   together and form a symmetic matrix used to compute the duplicity probability. It is a utility function and it is
#'   not accesible from outside the package.
#'
#' @param a list of data.table objects returned by each worker node with the device IDs on the first two columns and the
#'   corresponding duplicity probability on the third column for (the pair of devices in the first two columns).
#'
#' @param a vector with device IDs.
#'
#' @return a data.table object with two columns: the device IDs and the corresponding duplicity probability for each
#'   device.
buildDuplicityTable1to1 <- function(res, devices, Pii) {
  ndevices <- length(devices)
  Pij <- (1 - Pii) / (ndevices - 1)    # priori prob. of duplicity 2:1
  alpha <- Pij / Pii
  
  matsim <- NULL
  for (i in 1:length(res)) {
    matsim <- rbind(matsim, res[[i]])
  }
  rm(res)
  
  matsim[lower.tri(matsim)] <- t(matsim)[lower.tri(matsim)]
  
  dupP.dt <-
    data.table(deviceID = devices, dupP = rep(0, ndevices))
  
  for (i in 1:ndevices) {
    ll.aux <- matsim[i, -i]
    dupP.dt[deviceID == devices[i], dupP := 1 - 1 / (1 + (alpha * sum(exp(ll.aux))))]
  }
  return (dupP.dt)
  
}