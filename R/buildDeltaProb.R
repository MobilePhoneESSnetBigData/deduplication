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
#' @param If dim is 1 this function computes the probability distribution for Delta X, if it is 2 for Delta Y.
#' 
#' @t The time instant index in the list of all time instants.
#' 
#' @return The probability distribution for Delta X or Delta Y.
#' 
#' @export
buildDeltaProb<-function(centroids, device1, device2, path, dim, t) {
  if( dim!=1 & dim!=2)
    stop('dim should be 1 or 2')
  
  posLoc1 <- readPostLocProb(path, device1)[,..t]
  posLoc2 <- readPostLocProb(path, device2)[,..t]
  if( dim == 1) {
    x<-unique(centroids$x)
    deltaX <- as.data.table(expand.grid(x,x))
    deltaX[,p1:=0]
    deltaX[,p2:=0]
    colnames(deltaX) <-c('x1', 'x2', 'p1', 'p2')
    pp1<-vector(length = nrow(deltaX))
    pp2<-vector(length = nrow(deltaX))
    for(i in 1:length(x)) {
      xx<-x[i]
      tiles1<-centroids[,x]==xx
      tiles2<-centroids[,x]==xx
      pp1[deltaX[,x1]==xx]<-sum(posLoc1[tiles1,])
      pp2[deltaX[,x2]==xx]<-sum(posLoc2[tiles2,])
    }
    deltaX[,p1:=pp1]
    deltaX[,p2:=pp2]
  }
  return(deltaX)
}