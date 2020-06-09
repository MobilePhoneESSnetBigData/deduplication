
#' @import data.table
#' @import destim
#' @import parallel
#' @import doParallel
#' @export
computeDuplicityBayesian <- function(method, deviceIDs, pairs4dupl, modeljoin, llik, P1 = NULL, Pii = NULL, init = TRUE){
  
  ndevices <- length(deviceIDs)
  jointEmissions <- emissions(modeljoin)
  noEvents <- apply(jointEmissions, 2, sum)
  colNamesEmissions <- colnames(jointEmissions)
  keepCols <- names(pairs4dupl)[-which(names(pairs4dupl) %in% c("index.x", "index.y"))]
  if(method == "pairs"){
    P2 <- 1 - P1                  
    alpha <- P2 / P1
    cl <- makeCluster(detectCores())
    registerDoParallel()
    ichunks<-clusterSplit(cl,1:nrow(pairs4dupl))
    clusterEvalQ(cl, library("destim"))
    clusterEvalQ(cl, library("data.table"))
    clusterExport(cl, c('pairs4dupl', 'keepCols', 'noEvents', 'modeljoin', 'colNamesEmissions', 'alpha', 'llik', 'init'), envir = environment())
    res<-clusterApplyLB(cl, ichunks, doPair, pairs4dupl, keepCols, noEvents, modeljoin, colNamesEmissions, alpha, llik, init) 
    stopCluster(cl)

    dup<-NULL
    for(i in 1:length(res))
      dup<-rbind(dup, res[[i]])
    rm(res)
    
    dup[, deviceID1 := deviceIDs[index.x]][, deviceID2 := deviceIDs[index.y]]
    dupProb.dt1 <- copy(dup[, .(deviceID1, deviceID2, dupP)])
    setnames(dup, c("deviceID1", "deviceID2"), c("deviceID2", "deviceID1"))
    dupProb.dt2 <- copy(dup[, .(deviceID1, deviceID2, dupP)])
    dupProb.dt <- rbindlist(list(dupProb.dt1, dupProb.dt2))
  }
  
  else if(method == "1to1"){
    #####               COMPUTE LOGLIK BAYESIAN APPROACH  (1 to 1)             #####
    
    # TECHDEBT: The computation is carried out by brute force with a double nested loop.
    #           Refactoring using sparsity and parallelization techniques is needed
    
    dupProb.dt <- data.table(deviceID = deviceIDs, oneP = rep(0, nDevices))
    ll.matrix <- matrix(0L, nrow = nDevices, ncol = nDevices)
    
    Pij <- (1 - Pii) / (nDevices - 1)    # priori prob. of duplicity 2:1
    alpha <- Pij / Pii
    
    incomp.mt <- matrix(0L, ncol = nDevices, nrow = nDevices) # pairs with no compatibility
    
    keepCols <- names(pairs4dup)[-which(names(pairs4dup) %in% c("index.x", "index.y"))]
    
    for (i in 1:nDevices){
      cat(paste0(i, ': '))
      for (j in 1:nDevices){
        if(i < j){
          cat(paste0(j, ', '))
          
          newevents <- sapply(pairs4dup[index.x == i & index.y == j, ..keepCols],
                              function(x) ifelse(!is.na(x), which (x == colnames(jointEmissions)), NA))
          
          if(all(is.na(newevents)) | any(noEvents[newevents[!is.na(newevents)]]==0)){
            llij <- Inf
          }
          #else {
          if(!all(is.na(newevents)) & all(noEvents[newevents[!is.na(newevents)]]!=0)){
            
            fitTry <- try(modeljoin_ij <- fit(modeljoin, newevents, init = init)) # ML estimation of transition probabilities
            if(inherits(fitTry, "try-error")){
              incomp.mt[i, j] <- 1
              llij <- Inf
            }else{
              llij <- logLik(modeljoin_ij, newevents)
            }
          }
          ll.aux_ij <- (ll[i]+ll[j]) - llij
          ll.matrix[i, j] <- ll.aux_ij
          ll.matrix[j, i] <- ll.aux_ij
        } # end if i<j
      } #end for j
      
      ll.aux <- ll.matrix[i, -i]
      oneP0 <- 1 / (1 + (alpha * sum(exp(ll.aux))))
      dupProb.dt[deviceID == deviceIDs[i],
                 oneP := oneP0]
      
      cat(".\n")
    } # end for i
    cat(' ok.\n')
    dupProb.dt <- dupProb.dt[, dupP := 1 - oneP]
    output <- list(dupProb.dt = dupProb.dt, incomp.mt = incomp.mt)
  }
  else {
    stop("Method unknown!")
  }
  
  allPairs<-data.table(t(combn(c(1:ndevices), 2)))
  setnames(allPairs, c("index.x", "index.y"))
  allPairs1 <- copy(allPairs)
  setnames(allPairs, c("index.x", "index.y"), c("index.y", "index.x"))
  setcolorder(allPairs, names(allPairs1))
  allPairs.dt <- rbindlist(list(allPairs1, allPairs))[, deviceID1 := deviceIDs[index.x]][, deviceID2 := deviceIDs[index.y]]
  allDupProb.dt <- merge(allPairs.dt[, .(deviceID1, deviceID2)], dupProb.dt, all.x = TRUE
                         , by = c("deviceID1", "deviceID2"))
  allDupProb.dt[is.na(dupP), dupP := 0]
  
  
  dupP.dt <- copy(allDupProb.dt)[, max(dupP), by = "deviceID1"]
  setnames(dupP.dt, c("deviceID1", "V1"), c("deviceID", "dupP"))
  
  return(dupP.dt)
}


doPair<-function(ichunks, pairs, keepcols, noEvents, modeljoin, colNamesEmissions, alpha, llik, init) {
  
  localdup <- copy(pairs[ichunks, 1:2])
  for (i in 1:length(ichunks)){
    index.x0 <- localdup[i, index.x]
    index.y0 <- localdup[i, index.y]
    newevents <- sapply(pairs[ichunks[[i]], ..keepcols], function(x) ifelse(!is.na(x), which (x == colNamesEmissions), NA))
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
