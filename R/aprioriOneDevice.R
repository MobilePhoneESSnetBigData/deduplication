

#' @export
aprioriOneDeviceProb <- function(prob2Devices, ndevices) {
  n_ext <- round((1 - prob2Devices) * ndevices)  
  Pii <- (2*n_ext - ndevices)/(n_ext)  # priori probability of 1:1
  return(Pii)
}