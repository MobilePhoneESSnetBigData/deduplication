
#' @import data.table
#' @import destim
#' 
computeDuplicityBayesian <- function(method = c("pairs", "1to1"), deviceIDs, pairs4duplicity,
                                       P1 = NULL, Pii = NULL, modeljoin, logLik, init = TRUE){
  
  ndevices <- length(deviceIDs)
  new_eLoc_rCell.mt <- emissions(modeljoin)
  checkE <- apply(new_eLoc_rCell.mt, 2, sum)
  pairs4dup <- copy(pairs4duplicity)
  ll <- logLik
  
  if(method == "pairs"){
    dupProb.mt <- matrix(0L, ncol = ndevices, nrow = ndevices)
    
    
    P2 <- 1 - P1                                # priori prob. of 1:1
    alpha <- P2 / P1
    
    incomp.mt <- matrix(0L, ncol = ndevices, nrow = ndevices) # pairs with no compatibility
    
    keepCols <- names(pairs4dup)[-which(names(pairs4dup) %in% c("index.x", "index.y"))]
    
    
    for (i in 1:nrow(pairs4dup)){
      
      cat(paste0(i, ', '))
      
      index.x0 <- pairs4dup[i, index.x]
      index.y0 <- pairs4dup[i, index.y]
      
      newevents <- sapply(pairs4dup[i, ..keepCols], function(x) ifelse(!is.na(x), which (x == colnames(new_eLoc_rCell.mt)), NA))
      
      if(all(is.na(newevents)) | any(checkE[newevents[!is.na(newevents)]]==0)){
        llij <- Inf
      }
      if(!all(is.na(newevents)) & all(checkE[newevents[!is.na(newevents)]]!=0)){
        
        fitTry <- try(modeljoin_ij <- fit(modeljoin, newevents, init = init)) # ML estimation of transition probabilities
        if(inherits(fitTry, "try-error")){
          
          incomp.mt[index.x0, index.y0] <- 1
          llij <- Inf
          
        }else{
          llij <- logLik(modeljoin_ij, newevents)
        }
      }
      
      dupP0 <- 1 / (1 + (alpha * exp(llij - ll[index.x0] - ll[index.y0])))
      pairs4dup[index.x == index.x0 & index.y == index.y0, dupP := dupP0]
      
    } # end for i
    cat(' ok.\n')
    
    
    pairs4dup[, deviceID1 := deviceIDs[index.x]][, deviceID2 := deviceIDs[index.y]]
    dupProb.dt1 <- copy(pairs4dup[, .(deviceID1, deviceID2, dupP)])
    setnames(pairs4dup, c("deviceID1", "deviceID2"), c("deviceID2", "deviceID1"))
    dupProb.dt2 <- copy(pairs4dup[, .(deviceID1, deviceID2, dupP)])
    dupProb.dt <- rbindlist(list(dupProb.dt1, dupProb.dt2))
    
    output <- list(dupProb.dt = dupProb.dt, incomp.mt = incomp.mt)
    
  }
  
  if(method == "1to1"){
    ####  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ####
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
                              function(x) ifelse(!is.na(x), which (x == colnames(new_eLoc_rCell.mt)), NA))
          
          if(all(is.na(newevents)) | any(checkE[newevents[!is.na(newevents)]]==0)){
            llij <- Inf
          }
          if(!all(is.na(newevents)) & all(checkE[newevents[!is.na(newevents)]]!=0)){
            
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
  
  return(output)
  
}
