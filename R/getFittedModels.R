#' @title Fits the HMM models for each device.
#'
#' @description Fits the HMM model for each device using the \code{fit()} function from \code{destim} package. The
#'   computations are done in parallel to reduce the running time using all the available cores. This function creates a
#'   cluster of working nodes, splits the devices equally and assign a partition of devices to each node in the cluster.
#'   For Unix-like operating systems, this functions uses a "FORK" cluster while for Windows it uses a "SOCK" cluster.
#'
#' @param ndevices The number of devices.
#'
#' @param model The HMM model returned by \code{getGenericModel()} function. This model is to be fit for each device.
#'
#' @param connections A matrix whose elements are the antenna ID where a device is connected at every time instant. This
#'   matrix is returned by \code{getConnections()} function.
#'
#'
#' @return A vector of fitted model for each device.

#' @import parallel
#' @import destim
#' @import Rsolnp
#' @export
getFittedModels <-
  function(ndevices, model, connections) {
    cl<-buildCluster(c('connections', 'model'), c('destim'), env = environment())
    ichunks <- clusterSplit(cl, 1:ndevices)
    res <- clusterApplyLB(cl, ichunks, doFit2, model, connections)
    stopCluster(cl)
    result <- NULL
    for(i in 1:length(res))
      result <-c(result, res[[i]])
    return (result)
  }


doFit2 <- function(index, model, connections) {
  local_ll <- list(length = length(index))
  k = 1
  for (j in index) {
    modeli <- fit(model, connections[j, ], init = TRUE,  method = "solnp")
    local_ll[[k]] <- modeli
    k <- k + 1
  }
  return (local_ll)
}
