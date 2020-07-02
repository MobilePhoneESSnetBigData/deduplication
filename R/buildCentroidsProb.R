#' @title  build a concatenation between centroids and location probabilities
#' 
#' @export
buildCentroidProbs <-function(centroids, postLoc, t) {
  
  xp<-list(length = length(postLoc))
  for(i in 1:length(postLoc)) {
    xp[[i]]<-cbind(centroids[,.(x,y)], postLoc[[i]][,t])
  }

  return (xp)
  
}