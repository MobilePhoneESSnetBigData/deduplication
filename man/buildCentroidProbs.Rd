% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildCentroidProbs.R
\name{buildCentroidProbs}
\alias{buildCentroidProbs}
\title{Builds a concatenation between tiles' centroids and location probabilities.}
\usage{
buildCentroidProbs(centroids, postLoc, t)
}
\arguments{
\item{centroids}{The centroids of the tiles in the grid. The centroids are 
computed by calling \code{buildCentroids()} function.}

\item{postLoc}{The posterior location probabilities for each tile.}

\item{t}{The time instant for which the function uses the posterior 
location probabilities.}
}
\value{
A data.table object with 3 columns: x and y coordinates of the 
centroid of a tile and the location probability for that tile at a specific 
time instant.
}
\description{
Builds a data.table object that is obtined by a concatenation 
between coordinates of the centroids of each tile and location probabilities fro 
that tile.
}
\keyword{internal}
