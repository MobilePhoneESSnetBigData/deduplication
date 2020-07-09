buildDeltaX <-function(centroids, dim) {
  
  x <- unique(centroids[, ..dim][[1]])
  deltaX <- as.data.table(expand.grid(x, x))
  colnames(deltaX) <- c('x1', 'x2')
  deltaX[, dx := x1 - x2]
  return (deltaX)
}