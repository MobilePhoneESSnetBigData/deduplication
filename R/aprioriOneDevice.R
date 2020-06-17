#' @title Apriori probability of a person to have one mobile device.
#'
#' @description Apriori probability of a person to have one mobile device. It is computed simply as (2*Ndev1-Ndev)/Ndev1 where
#' Ndev1 is the number of devices that are in a 1-to-1 correspondence with the persons that hold them.
#'
#'@param prob2Devices The probability of a person to have 2 devices. In case of using simulated data, this parameter is read
#'from the simulation configuration file.
#'
#'@param ndevices The number of devices detected by the network during the time horizon under consideration.
#'
#' @export
aprioriOneDeviceProb <- function(prob2Devices, ndevices) {
  n_ext <- round((1 - prob2Devices) * ndevices)
  Pii <-
    (2 * n_ext - ndevices) / (n_ext)  # priori probability of 1:1
  return(Pii)
}