#' @title  build a concatenation between centroids and location probabilities
#' 
#' @export
buildCentroidProbs <-function(centroids, postLoc, t) {
  
  xp<-list(length = length(postLoc))
  for(i in 1:length(postLoc)) {
    tmp<-cbind(centroids[,.(x,y)], postLoc[[i]][,t])
    #coln<-colnames(tmp)
    tmp<-tmp[V2!=0][,]
    xp[[i]]<-tmp
  }

  return (xp)
  
}