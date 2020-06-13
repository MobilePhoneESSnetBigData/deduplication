
#' @import data.table
#' @import destim
#' @import parallel
#' @import doParallel
#' @include splitReverse.R
#' @export
computeDuplicityBayesian <- function(method, deviceIDs, pairs4dupl, modeljoin, llik, P1 = NULL, Pii = NULL, init = TRUE){
  
  ndevices <- length(deviceIDs)
  jointEmissions <- emissions(modeljoin)
  noEvents <- apply(jointEmissions, 2, sum)
  colNamesEmissions <- colnames(jointEmissions)
  eee0 <- 1:length(colNamesEmissions)
  envEmissions <-new.env(hash = TRUE)
  for(i in eee0)
    envEmissions[[colNamesEmissions[i]]] <- i
  
  keepCols <- names(pairs4dupl)[-which(names(pairs4dupl) %in% c("index.x", "index.y"))]
  if(method == "pairs"){
    P2 <- 1 - P1                  
    alpha <- P2 / P1
    if ( Sys.info()[['sysname']] == 'Linux' | Sys.info()[['sysname']] == 'Darwin') {
      cl <- makeCluster(detectCores(), type = "FORK")
    } else {
      cl <- makeCluster(detectCores())
      clusterEvalQ(cl, library("destim"))
      clusterEvalQ(cl, library("data.table"))
      clusterExport(cl, c('pairs4dupl', 'keepCols', 'noEvents', 'modeljoin', 'envEmissions', 'alpha', 'llik', 'init'), envir = environment())
    }
    ichunks<-clusterSplit(cl,1:nrow(pairs4dupl))
    res<-clusterApplyLB(cl, ichunks, doPair, pairs4dupl, keepCols, noEvents, modeljoin, envEmissions, alpha, llik, init) 
    stopCluster(cl)
    
    dup <- NULL
    for(i in 1:length(res))
      dup<-rbind(dup, res[[i]])
    rm(res)
    
    dup[, deviceID1 := deviceIDs[index.x]][, deviceID2 := deviceIDs[index.y]]
    dup2<-copy(dup)
    dup2[, deviceID1 := deviceIDs[index.y]][, deviceID2 := deviceIDs[index.x]]
    dup<-dup[,.(deviceID1, deviceID2, dupP)]        
    dup2<-dup2[,.(deviceID1, deviceID2, dupP)]        
    dupProb.dt <- rbindlist(list(dup, dup2))
    
    allPairs<-expand.grid(devices, devices)
    rows_to_keep<-allPairs[,1]!=allPairs[,2]
    allPairs<-as.data.table(allPairs[rows_to_keep,])
    setnames(allPairs, c(c("deviceID1", "deviceID2")))
    allDupProb.dt <- merge(allPairs[, .(deviceID1, deviceID2)], dupProb.dt, all.x = TRUE
                           , by = c("deviceID1", "deviceID2"))
    allDupProb.dt[is.na(dupP), dupP := 0]
    
    dupP.dt <- copy(allDupProb.dt)[, max(dupP), by = "deviceID1"]
    setnames(dupP.dt, c("deviceID1", "V1"), c("deviceID", "dupP"))
  }
  
  else if(method == "1to1"){
    dupP.dt <- data.table(deviceID = deviceIDs, dupP = rep(0, ndevices))
    Pij <- (1 - Pii) / (ndevices - 1)    # priori prob. of duplicity 2:1
    alpha <- Pij / Pii
    
    if ( Sys.info()[['sysname']] == 'Linux' | Sys.info()[['sysname']] == 'Darwin') {
      cl <- makeCluster(detectCores(), type = "FORK")
    } else {
      cl <- makeCluster(detectCores())
      clusterEvalQ(cl, library("destim"))
      clusterEvalQ(cl, library("data.table"))
      clusterExport(cl, c('pairs4dupl', 'devices', 'keepCols', 'noEvents', 'modeljoin', 'colNamesEmissions', 'alpha', 'llik', 'init'), envir = environment())
    }
    ichunks<-clusterSplit(cl,1:ndevices)
    #ichunks<-splitReverse(ndevices, detectCores())
    # res<-list()
    # for(k in 1:length(ichunks)) {
    #   res[[k]]<-do1to1(ichunks[[k]], pairs4dupl, devices, keepCols, noEvents, modeljoin, colNamesEmissions, alpha, llik, init)
    # }
    res<-clusterApply(cl, ichunks, do1to1, pairs4dupl, devices, keepCols, noEvents, modeljoin, envEmissions, alpha, llik, init) 
    stopCluster(cl)
    
    matsim <- NULL
    for(i in 1:length(res)){
      matsim<-rbind(matsim, res[[i]])
    }
    rm(res)
    matsim[lower.tri(matsim)]<-t(matsim)[lower.tri(matsim)]
    for(i in 1:ndevices) {
      ll.aux <- matsim[i, -i]
      dupP.dt[deviceID == devices[i], dupP := 1-1 / (1 + (alpha * sum(exp(ll.aux))))]
    }
  } else {
    stop("Method unknown!")
  }
  return(dupP.dt)
}


do1to1 <-function(ichunks, pairs, devices, keepCols, noEvents, modeljoin, envEms, alpha, llik, init) {
  ndev<-length(devices)  
  ll.matrix <- matrix(0L, nrow = length(ichunks), ncol = ndev)
  
  for(i in 1:(length(ichunks))) {
    ii<-ichunks[[i]]
    #cat(paste0(ichunks[[i]], ': '))
    if((ii + 1) <= ndev)
      j_list<-(ii+1):ndev
    else
      j_list <-c()
    for (j in j_list){
      #cat(paste0(j, ', '))
      newevents <- sapply(pairs[index.x == ii & index.y == j, ..keepCols],
                          function(x) ifelse(!is.na(x), envEms[[x]], NA))
      
      if(all(is.na(newevents)) | any(noEvents[newevents[!is.na(newevents)]]==0)){
        llij <- Inf
      } else {
          fitTry <- try(modeljoin_ij <- fit(modeljoin, newevents, init = init)) # ML estimation of transition probabilities
          if(inherits(fitTry, "try-error")){
            llij <- Inf
          } else {
            llij <- logLik(modeljoin_ij, newevents)
          }
      }
      ll.aux_ij <- llik[ii]+llik[j] - llij
      ll.matrix[i, j] <- ll.aux_ij
    } #end for j
    cat(".\n")
  }
  return(ll.matrix)
}

doPair<-function(ichunks, pairs, keepcols, noEvents, modeljoin, envEms, alpha, llik, init) {
  
  localdup <- copy(pairs[ichunks, 1:2])
  for (i in 1:length(ichunks)){
    index.x0 <- localdup[i, index.x]
    index.y0 <- localdup[i, index.y]
    newevents <- sapply(pairs[ichunks[[i]], ..keepcols], function(x) ifelse(!is.na(x), envEms[[x]], NA))
    if(all(is.na(newevents)) | any(noEvents[newevents[!is.na(newevents)]]==0)){
      llij <- Inf
    } else {
      fitTry <- try( modeljoin_ij <- fit(modeljoin, newevents, init = init)) # ML estimation of transition probabilities
      if(inherits(fitTry, "try-error")) {
        llij <- Inf
      } else {
        llij <- logLik(modeljoin_ij, newevents)
      }
    }
    dupP0 <- 1 / (1 + (alpha * exp(llij - llik[index.x0] - llik[index.y0])))
    localdup[index.x == index.x0 & index.y == index.y0, dupP := dupP0]
  }
  return(localdup)
}
