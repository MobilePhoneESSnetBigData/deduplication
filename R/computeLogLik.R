


computeLogLik <- function(times, devices, model, connections) {
  
    ndev = length(devices)
    ll <- vector(length = ndev)
    for(i in 1:ndev){
      cat(paste0(i, ", "))
      devIDi <- devices[i]
      model <- fit(model, init = TRUE, connections[i,]  )
      ll[i] <- logLik(model, connections[i,])
    }
    return (ll)
}