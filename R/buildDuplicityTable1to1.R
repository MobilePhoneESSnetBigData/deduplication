



buildDuplicityTable1to1 <- function(res, devices) {
  ndevices <- lenght(devices)
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