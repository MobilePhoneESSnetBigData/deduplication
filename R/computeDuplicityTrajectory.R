#' @title Computes the duplicity probabilities for each device using the trajectory approach.
#'
#'
#'
#'
#' @include centerOfProbabilities.R
#' @include readPostLocProb.R
#' @include dispersionRadius.R
#' @include buildCentroidProbs.R
#' @include buildCentroids.R
#' @include buildDeltaProb.R
#' @import data.table
#' @import parallel
#' @export
computeDuplicityTrajectory <-function(path, devices, gridParams, pairs, P1 , T, gamma) {
  devices <- sort(as.numeric(devices))
  centrs <- buildCentroids(gridParams$ncol, gridParams$nrow, gridParams$tileX, gridParams$tileY)
  ndevices <- length(devices)
  
  postLoc <- NULL
  centerOfProbs <- NULL
  dr<-NULL
  cpp<-NULL
  
  P2 <- 1 - P1
  alpha<-P1/P2

  ### from this point the code should go parallel  
  
  cl <- buildCluster( c('path', 'devices', 'centrs', 'centerOfProbs'), env = environment())
  ichunks <- clusterSplit(cl, 1:ndevices)
  res <- clusterApplyLB( cl, ichunks, doLocations, path, devices, centrs )
  for(i in 1:length(res)) {
    postLoc <- c(postLoc, res[[i]]$postLoc)
    centerOfProbs <- c(centerOfProbs, res[[i]]$centerOfProbs)
    dr <- c(dr, res[[i]]$dr)
  }
  rm(res)

  ichunks2 <- clusterSplit(cl, 1:T)
  clusterExport(cl, varlist = c('postLoc'), envir = environment())
  res <- clusterApplyLB( cl, ichunks2, doCPP, centrs, postLoc )
  for(i in 1:length(res)) {
    cpp <- c(cpp, res[[i]])
  }
  rm(res)


  pairs<-pairs[,1:2]
  ichunks3 <- clusterSplit(cl, 1:nrow(pairs))
  
  clusterExport(cl, varlist = c('pairs', 'cpp', 'dr', 'ndevices', 'T', 'gamma', 'alpha'), envir = environment())
  res<-clusterApplyLB(cl, ichunks3, doPairs, pairs, cpp, ndevices, dr, T, alpha, gamma)
  stopCluster(cl)
  dup<-buildDuplicityTablePairs(res, devices)
  return (dup)
  
}

doLocations <- function(ichunks, path, devices, centrs, centerOfProbs ) {
  n <- length(ichunks)
  local_postLoc<-list(length = n)
  local_centerOfProbs<-list(length = n)
  local_dr <- list(length = n)
  k<-1
  for( i in ichunks) {
    local_postLoc[[k]] <- readPostLocProb(path, devices[i])
    local_centerOfProbs[[k]] <- centerOfProbabilities(centrs, local_postLoc[[k]])
    local_dr[[k]] <- dispersionRadius(centrs, local_postLoc[[k]], local_centerOfProbs[[k]])
    k <- k + 1
  }
  res<-list(postLoc = local_postLoc, centerOfProbs = local_centerOfProbs, dr = local_dr )
  return (res)
}

doCPP <- function(ichunks, centrs, postLoc) {
  n <- length(ichunks)
  local_cpp <- list(length = n)
  k<-1
  for ( i in ichunks) {
     local_cpp[[k]]<-buildCentroidProbs(centrs, postLoc, i)
     k<-k+1
  }
  return (local_cpp)
}


doPairs <- function(ichunks, pairs, cpp, ndev, dr, T, alpha, gamma) {
  n <- length(ichunks)  
  localdup <- copy(pairs[ichunks, 1:2])
  for( i in ichunks ) {
    index_i <-pairs[i,1][[1]]
    index_j <-pairs[i,2][[1]]
    s1<-0
    for(t in 1:T) {
      mm<-0.5*max(dr[[index_i]][t], dr[[index_j]][t])
      mdelta<-buildDeltaProb(cpp[[t]][[index_i]], cpp[[t]][[index_j]])
      s1<-s1+(abs(modeDelta(mdelta[[1]]))<mm & abs(modeDelta(mdelta[[2]]))<mm)
    }
    tmp <- s1/T
    dupP0 <- 1 - 1/(1+alpha*tmp/(1-tmp))
    localdup[index.x == index_i &index.y == index_j, dupP := dupP0]
  }
  return (localdup)
}