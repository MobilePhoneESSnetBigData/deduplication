#' @import parallel
#' @import doParallel
#' @export
fitModels <-function(ndevices, model, connections, parallel = TRUE) {
  
  if(parallel == FALSE) {
    #serial computations
    ll <- vector(length = ndevices)
    for( i in 1:ndevices) {
      modeli <- fit(model, init = TRUE, connections[i,]  )
      ll[i] <- logLik(modeli, connections[i,])
    }
    return (ll)
  }
  else if( parallel == TRUE) {
    cl <- makeCluster(detectCores())
    registerDoParallel()
    ichunks<-clusterSplit(cl,1:ndevices)
    clusterEvalQ(cl, library("destim"))
    clusterExport(cl, c('connections', 'model'))
    res<-clusterApplyLB(cl, ichunks, doFit, model, connections) 
    stopCluster(cl)
    return (unlist(res))
  }
}


doFit <-function(index, model, connections) {
  local_ll <- vector(length = length(index))
  k=1
  for(j in index) {
    modeli <- fit(model, init = TRUE, connections[j,] )
    local_ll[k] <- logLik(modeli, connections[j,])
    k <- k + 1
  }
  return (local_ll)
}

