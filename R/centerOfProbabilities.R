#' @title Computes the center of probabilities (as center of mass) for each posterior probability
#'   cloud at each time instant, for a device.
#'
#' @description Computes the center of probabilities (as center of mass) for each posterior
#'   probability cloud at each time instant, for a device. It uses the results obtained from
#'   \code{destim}.
#'
#' @param centroid A data.table object with the centroids for each tile of the grid.
#' @param postLocProb a Matrix object with the posterior location probabilities computed by
#'   \code{destim}. The number of rows equals the number of tiles and the number of columns equals
#'   the number of successive time instants. An element \code{postLocProb[i,j]} represents the
#'   posterior location probability corresponding to tile \code{i} at the time instant \code{j}.
#'
#' @return
#'
#' @import data.table
#' @export
centerOfProbabilities <- function(centroid, postLocProb) {
  nc <- nrow(centroid)
  nw <- length(postLocProb)
  if (nw != 1 && nw != nc)
    stop('postLocProb must have values for all tiles.')
  if (nw == 1 && w * nc != 1)
    stop(
      'in case all tiles have the same posterior location probability, the sum of all these values must be 1'
    )
  
  cp_x <- vector(0L, length = ncol(postLocProb))
  cp_y <- vector(0L, length = ncol(postLocProb))
  for (i in 1:ncol(postLocProb)) {
    cp_x[i] <-  sum(postLocProb[, i] * centroid[, 1]) / sum(postLocProb[, i])
    cp_y[i] <-sum(postLocProb[, i] * centroid[, 2]) / sum(postLocProb[, i])
  }
  cp <- cbind(cp_x, cp_y)
  names(cp) <- c('centerProb_x', 'centerProb_y')
  return(cp)
  
}