#' @title Computes the probability distribution for Delta X and Delta Y
#'
#' @description Computes the probability distribution for Delta X and Delta Y where Delta X / Delta Y are the differences
#' between the centroids of the tiles on OX / OY. The location probability for each tile comes from \code{destim}.
#'
#' @param centroids The centroids of all tiles in the grid.
#'
#' @param device1 The ID of the first device.
#'
#' @param device2 The ID of the second device.
#'
#' @param path The path to the location where the posterior location probabilities are stored.
#'
#' @param dim If dim is 1 this function computes the probability distribution for Delta X, if it is 2 for Delta Y.
#'
#' @return The probability distribution for Delta X or Delta Y.
#'
#' @import Matrix
#' @import data.table
#' @export
buildDeltaProb <-
  function(delta, postLocProbDevice1, postLocProbDevice2) {
    # if (dim != 1 & dim != 2)
    #   stop('dim should be 1 or 2')
    # x <- unique(centroids[, ..dim][[1]])
    # deltaX <- as.data.table(expand.grid(x, x))
    # colnames(deltaX) <- c('x1', 'x2')
    pp1 <- vector(length = nrow(delta))
    pp2 <- vector(length = nrow(delta))
    x <- unique(delta[, x1])
    for (i in 1:length(x)) {
      #xx <- x[i]
      tiles1 <- centroids[, x] == x[i]
      pp1[deltaX[, x1] == x[i]] <- sum(postLocProbDevice1[tiles1])
      pp2[deltaX[, x2] == x[i]] <- sum(postLocProbDevice2[tiles1])
    }
    deltaX[, p := pp1 * pp2]
    #deltaX[, dx := x1 - x2]
    deltaX <- deltaX[, .(dx, p)]
    deltaX[, lapply(.SD,sum), by=.(dx)]
    
    # ddx <- unique(deltaX[, dx])
    # pDx <- NULL
    # 
    # for (i in 1:length(ddx)) {
    #   pDx <- c(pDx, deltaX[dx == ddx[i], sum(p)])
    # }
    result <- deltaX 
    #data.table(delta = ddx, p = pDx)
    
    return(result)
  }


buildDeltaProb2 <-
  function(delta, postLocProbDevice1, postLocProbDevice2) {
    pp1 <- vector(length = nrow(delta))
    pp2 <- vector(length = nrow(delta))
    x <- unique(delta[, x1])
    for (i in 1:length(x)) {
      #xx <- x[i]
      tiles1 <- centroids[, x] == x[i]
      pp1[deltaX[, x1] == x[i]] <- sum(postLocProbDevice1[tiles1])
      pp2[deltaX[, x2] == x[i]] <- sum(postLocProbDevice2[tiles1])
    }
    deltaX[, p := pp1 * pp2]
    deltaX <- deltaX[, .(dx, p)]
    deltaX[, lapply(.SD,sum), by=.(dx)]
    return(deltaX)
  }

buildDeltaProb3 <-
  function(delta, postLocProbDevice1, postLocProbDevice2) {
    pp1 <- vector(length = nrow(delta))
    pp2 <- vector(length = nrow(delta))
    x <- unique(delta[, x1])
    for (i in 1:length(x)) {
      tiles1 <- centroids[, y] == x[i]
      pp1[deltaX[, x1] == x[i]] <- sum(postLocProbDevice1[tiles1])
      pp2[deltaX[, x2] == x[i]] <- sum(postLocProbDevice2[tiles1])
    }
    deltaX[, p := pp1 * pp2]
    deltaX <- deltaX[, .(dx, p)]
    deltaX[, lapply(.SD,sum), by=.(dx)]
    return(deltaX)
  }

buildDeltaProb4 <-function(centroids, postLocProbDevice1, postLocProbDevice2, dim) {
  if(dim==1) {
    xp1<-cbind(centroids[,.(x)], postLocProbDevice1)
    xp2<-cbind(centroids[,.(x)], postLocProbDevice2)
  } else {
    xp1<-cbind(centroids[,.(y)], postLocProbDevice1)
    xp2<-cbind(centroids[,.(y)], postLocProbDevice2)
  }
  colnames(xp1)<-c('x1', 'p1')
  colnames(xp2)<-c('x2', 'p2')
  
  xp1<-xp1[p1!=0][,]
  xp2<-xp2[p2!=0][,]
  deltaX <- CJ.table(xp1,xp2)
  deltaX <- deltaX[, dx:=x1-x2]
  deltaX <- deltaX[, p:=p1*p2]
  deltaX <- deltaX[, .(dx, p)]
  deltaX[, lapply(.SD,sum), by=.(dx)]
  return(deltaX)
}


buildDeltaProb5 <-function(centroids, postLocProbDevice1, postLocProbDevice2) {

  xp1<-cbind(centroids[,.(x,y)], postLocProbDevice1)
  xp2<-cbind(centroids[,.(x,y)], postLocProbDevice2)

  colnames(xp1)<-c('x1', 'y1', 'p1')
  colnames(xp2)<-c('x2', 'y2', 'p2')
  
  xp1<-xp1[p1!=0][,]
  xp2<-xp2[p2!=0][,]
  deltaX <- CJ.table(xp1[,.(x1,p1)],xp2[,.(x2,p2)])
  deltaX <- deltaX[, delta:=x1-x2]
  deltaX <- deltaX[, p:=p1*p2]
  deltaX <- deltaX[, .(delta, p)]
  deltaX<-deltaX[, lapply(.SD,sum), by=.(delta)]
  
  deltaY <- CJ.table(xp1[,.(y1,p1)],xp2[,.(y2,p2)])
  deltaY <- deltaY[, delta:=y1-y2]
  deltaY <- deltaY[, p:=p1*p2]
  deltaY <- deltaY[, .(delta, p)]
  deltaY<-deltaY[, lapply(.SD,sum), by=.(delta)]
  
  return(list(deltaX=deltaX, deltaY=deltaY))
}


CJ.table <- function(X,Y) {
   setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]
}

CJ.table.2 <- function(X,Y) {
     eval(parse(text=paste0("setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],list(",paste0(unique(c(names(X),names(Y))),collapse=","),"),by=.EACHI][,k:=NULL]")))
}
