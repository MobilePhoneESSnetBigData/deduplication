
aprioriDuplicityProb <- function(prob2Devices, ndevices) {
  
  n_ext <- round((1 - prob2Devices) * ndevices)
  P1 <- (2*(ndevices-n_ext))/(ndevices*(ndevices - 1))
  return (P1)
}