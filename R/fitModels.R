#' @title Fits the HMM models for each device.
#' 
#' @description Fits the HMM model for each device usin the \code{fit()} function from \code{destim} package.
#' 
#' @param ndevices The number of devices.
#' 
#' @param 
#' 
#' @param 
#' 
#' @param 
#' 
#' @return 

#' @import parallel
#' @import doParallel
#' @import destim
#' @export
fitModels <-
  function(ndevices, model, connections, parallel = TRUE) {
    if (parallel == FALSE) {
      #serial computations
      ll <- vector(length = ndevices)
      for (i in 1:ndevices) {
        modeli <-
          fit(model, connections[i, ], init = TRUE, method = "solnp")
        ll[i] <- logLik(modeli, connections[i, ])
      }
      return (ll)
    }
    else if (parallel == TRUE) {
      if (Sys.info()[['sysname']] == 'Linux' |
          Sys.info()[['sysname']] == 'Darwin') {
        cl <- makeCluster(detectCores(), type = "FORK")
      } else {
        cl <- makeCluster(detectCores())
        clusterEvalQ(cl, library("destim"))
        clusterExport(cl, c('connections', 'model'))
      }
      ichunks <- clusterSplit(cl, 1:ndevices)
      res <- clusterApplyLB(cl, ichunks, doFit, model, connections)
      stopCluster(cl)
      return (unlist(res))
    }
  }


doFit <- function(index, model, connections) {
  local_ll <- vector(length = length(index))
  k = 1
  for (j in index) {
    modeli <-
      fit(model, connections[j, ], init = TRUE,  method = "solnp")
    local_ll[k] <- logLik(modeli, connections[j, ])
    k <- k + 1
  }
  return (local_ll)
}
