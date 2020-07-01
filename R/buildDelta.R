buildDelta <- function(centroids, dim) {
  
  x <- unique(centroids[, ..dim][[1]])
  deltaX <- as.data.table(expand.grid(x, x))
  deltaX[, dx := Var1 - Var2]
  colnames(deltaX) <- c('x1', 'x2', 'dx')
  return (deltaX)
  
}