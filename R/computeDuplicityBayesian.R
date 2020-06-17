#' @title Computes the duplicity probabilities for each device using a Bayesian approach.
#'
#' @description Computes the duplicity probabilities for each device using a Bayesian approach. Th
#'
#' @param method. Selects a method to compute the duplicity probabilities. It could have one of the two values: "pairs"
#'   or "1to1". When selecting "pairs" method, the pairs4dupl parameter contains only the compatible pairs of devices,
#'   i.e. the pairs that most of the time are connected to the same or to neighbouring antennas. "1to1" method checks
#'   all posible combinations between devices to compute the duplicity probabilities.
#'
#'
#' @param deviceIDs A vector with the all device IDs. It is obtained by caliing the \code{getDevices()} function.
#'
#' @param pairs4dupl A data.table object with pairs of devices and pairs of antennas where these devices are connecting.
#' It can be obtained by calling \code{computePairs()} function. 
#'
#' @param modeljoin The joint HMM model returned by \code{getJointModel()} function.

#' @param llik A vector with the values of the log likelihood after the individual HMM models for each device were
#'   fitted. This vector can be obtained by calling \code{fitModels()} function.
#'
#' @param P1 The apriori duplicity probility as it is returned by \code{aprioriDuplicityProb()} function. It is used
#'   when "pairs" method is selected.
#'
#' @param Pii Apriori probability of a device to be in a 1-to-1 correspondence with the holder as it is returned by
#'   \code{aprioriOneDeviceProb()} function. This parameter is used only when "1to1" method is selected.
#'
#' @param init A logical value. If TRUE, the \code{fit()} function uses the stored steady state as fixed initialization,
#'   otherwise the steady state is computed at every call of \code{fit()} function.
#'
#' @return a data.table object with two columns: 'deviceID' and 'dupP'. On the first column there are deviceIDs and on
#'   the second column the corresponding duplicity probability, i.e. the probability that a device is in a 2-to-1
#'   correspondence with the holder.
#'
#' @import data.table
#' @import destim
#' @import parallel
#' @import doParallel
#' @export
computeDuplicityBayesian <-
  function(method,
           deviceIDs,
           pairs4dupl,
           modeljoin,
           llik,
           P1 = NULL,
           Pii = NULL,
           init = TRUE) {

    ndevices <- length(deviceIDs)
    jointEmissions <- emissions(modeljoin)
    noEvents <- apply(jointEmissions, 2, sum)
    colNamesEmissions <- colnames(jointEmissions)
    eee0 <- 1:length(colNamesEmissions)
    envEmissions <- new.env(hash = TRUE)
    
    for (i in eee0)
      envEmissions[[colNamesEmissions[i]]] <- i
    
    keepCols <-
      names(pairs4dupl)[-which(names(pairs4dupl) %in% c("index.x", "index.y"))]
    
    if (method == "pairs") {
      P2 <- 1 - P1
      alpha <- P2 / P1
      if (Sys.info()[['sysname']] == 'Linux' |
          Sys.info()[['sysname']] == 'Darwin') {
        cl <- makeCluster(detectCores(), type = "FORK")
      } else {
        cl <- makeCluster(detectCores())
        clusterEvalQ(cl, library("destim"))
        clusterEvalQ(cl, library("data.table"))
        clusterExport(
          cl,
          c(
            'pairs4dupl',
            'keepCols',
            'noEvents',
            'modeljoin',
            'envEmissions',
            'alpha',
            'llik',
            'init'
          ),
          envir = environment()
        )
      }
      #cl <-buildCluster()
      ichunks <- clusterSplit(cl, 1:nrow(pairs4dupl))
      res <-
        clusterApplyLB(
          cl,
          ichunks,
          doPair,
          pairs4dupl,
          keepCols,
          noEvents,
          modeljoin,
          envEmissions,
          alpha,
          llik,
          init
        )
      stopCluster(cl)
      
      dup <- NULL
      for (i in 1:length(res))
        dup <- rbind(dup, res[[i]])
      rm(res)
      
      dup[, deviceID1 := deviceIDs[index.x]][, deviceID2 := deviceIDs[index.y]]
      dup2 <- copy(dup)
      dup2[, deviceID1 := deviceIDs[index.y]][, deviceID2 := deviceIDs[index.x]]
      dup <- dup[, .(deviceID1, deviceID2, dupP)]
      dup2 <- dup2[, .(deviceID1, deviceID2, dupP)]
      dupProb.dt <- rbindlist(list(dup, dup2))
      
      allPairs <- expand.grid(devices, devices)
      rows_to_keep <- allPairs[, 1] != allPairs[, 2]
      allPairs <- as.data.table(allPairs[rows_to_keep, ])
      setnames(allPairs, c(c("deviceID1", "deviceID2")))
      allDupProb.dt <-
        merge(
          allPairs[, .(deviceID1, deviceID2)],
          dupProb.dt,
          all.x = TRUE
          ,
          by = c("deviceID1", "deviceID2")
        )
      allDupProb.dt[is.na(dupP), dupP := 0]
      rm(dup)
      rm(dup2)
      dupP.dt <- copy(allDupProb.dt)[, max(dupP), by = "deviceID1"]
      rm(dupProb.dt)
      setnames(dupP.dt, c("deviceID1", "V1"), c("deviceID", "dupP"))
    }
    
    else if (method == "1to1") {
      Pij <- (1 - Pii) / (ndevices - 1)    # priori prob. of duplicity 2:1
      alpha <- Pij / Pii
      
      if (Sys.info()[['sysname']] == 'Linux' |
          Sys.info()[['sysname']] == 'Darwin') {
        cl <- makeCluster(detectCores(), type = "FORK")
      } else {
        cl <- makeCluster(detectCores())
        clusterEvalQ(cl, library("destim"))
        clusterEvalQ(cl, library("data.table"))
        clusterExport(
          cl,
          c(
            'pairs4dupl',
            'devices',
            'keepCols',
            'noEvents',
            'modeljoin',
            'colNamesEmissions',
            'alpha',
            'llik',
            'init'
          ),
          envir = environment()
        )
      }
      ichunks <- clusterSplit(cl, 1:ndevices)
      res <-
        clusterApplyLB(
          cl,
          ichunks,
          do1to1,
          pairs4dupl,
          devices,
          keepCols,
          noEvents,
          modeljoin,
          envEmissions,
          alpha,
          llik,
          init
        )
      stopCluster(cl)
      
      matsim <- NULL
      for (i in 1:length(res)) {
        matsim <- rbind(matsim, res[[i]])
      }
      rm(res)
      
      matsim[lower.tri(matsim)] <- t(matsim)[lower.tri(matsim)]
      dupP.dt <-
        data.table(deviceID = deviceIDs, dupP = rep(0, ndevices))
      for (i in 1:ndevices) {
        ll.aux <- matsim[i,-i]
        dupP.dt[deviceID == devices[i], dupP := 1 - 1 / (1 + (alpha * sum(exp(ll.aux))))]
      }
    } else {
      stop("Method unknown!")
    }
    return(dupP.dt)
  }


do1to1 <-
  function(ichunks,
           pairs,
           devices,
           keepCols,
           noEvents,
           modeljoin,
           envEms,
           alpha,
           llik,
           init) {
    ndev <- length(devices)
    n <- length(ichunks)
    ll.matrix <- matrix(0L, nrow = n, ncol = ndev)
    for (i in 1:n) {
      ii <- ichunks[[i]]
      llik_ii <- llik[ii]
      if ((ii + 1) <= ndev)
        j_list <- (ii + 1):ndev
      else
        j_list <- c()
      for (j in j_list) {
        newevents <- sapply(pairs[index.x == ii & index.y == j, ..keepCols],
                            function(x)ifelse(!is.na(x), envEms[[x]], NA))
        
        if (all(is.na(newevents)) |
            any(noEvents[newevents[!is.na(newevents)]] == 0)) {
          llij <- Inf
        } else {
          fitTry <-
            try(modeljoin_ij <-
                  fit(modeljoin,
                      newevents,
                      init = init,
                      method = "solnp"))
          if (inherits(fitTry, "try-error")) {
            llij <- Inf
          } else {
            llij <- logLik(modeljoin_ij, newevents)
          }
        }
        ll.aux_ij <- llik_ii + llik[j] - llij
        ll.matrix[i, j] <- ll.aux_ij
      } #end for j
    }
    return(ll.matrix)
  }

doPair <-
  function(ichunks,
           pairs,
           keepcols,
           noEvents,
           modeljoin,
           envEms,
           alpha,
           llik,
           init) {
    localdup <- copy(pairs[ichunks, 1:2])
    for (i in 1:length(ichunks)) {
      index.x0 <- localdup[i, index.x]
      index.y0 <- localdup[i, index.y]
      newevents <-
        sapply(pairs[ichunks[[i]], ..keepcols], function(x)
          ifelse(!is.na(x), envEms[[x]], NA))
      if (all(is.na(newevents)) |
          any(noEvents[newevents[!is.na(newevents)]] == 0)) {
        llij <- Inf
      } else {
        fitTry <-
          try(modeljoin_ij <-
                fit(modeljoin,
                    newevents,
                    init = init,
                    method = "solnp"))
        if (inherits(fitTry, "try-error")) {
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
