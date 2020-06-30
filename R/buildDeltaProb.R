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
  function(centroids, postLocProbDevice1, postLocProbDevice2, dim) {
    if (dim != 1 & dim != 2)
      stop('dim should be 1 or 2')
    T  <- nrow(postLocProbDevice1)
    #posLoc1 <- postLocProbDevice1[((t-1)*T+1):((t-1)*T+T)]
    #posLoc2 <- postLocProbDevice1[((t-1)*T+1):((t-1)*T+T)]
    posLoc1 <- postLocProbDevice1
    posLoc2 <- postLocProbDevice2
    x <- unique(centroids[, ..dim][[1]])
    deltaX <- as.data.table(expand.grid(x, x))
    colnames(deltaX) <- c('x1', 'x2')
    pp1 <- vector(length = nrow(deltaX))
    pp2 <- vector(length = nrow(deltaX))
    for (i in 1:length(x)) {
      xx <- x[i]
      tiles1 <- centroids[, x] == xx
      tiles2 <- centroids[, x] == xx
      pp1[deltaX[, x1] == xx] <- sum(posLoc1[tiles1])
      pp2[deltaX[, x2] == xx] <- sum(posLoc2[tiles2])
    }
    deltaX[, p := pp1 * pp2]
    deltaX[, dx := x1 - x2]
    deltaX <- deltaX[, .(dx, p)]
    ddx <- unique(deltaX[, dx])
    modeDx <- NULL
    for (i in 1:length(ddx)) {
      modeDx <- c(modeDx, deltaX[dx == ddx[i], sum(p)])
    }
    result <- data.table(delta = ddx, p = modeDx)
    
    return(result)
  }
