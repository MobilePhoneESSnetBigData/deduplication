#' @import data.table
#' @import rgeos
antennaNeighbours <- function(coverarea) {
  antennas <- coverArea[['antennaID']]
  y <-data.table(t(combn(antennas,2)))
  y1<-merge(y, coverArea, by.x='V1', by.y='antennaID', all.x = TRUE, sort = TRUE)
  rm(y)
  y2<-merge(y1, coverArea, by.x='V2', by.y='antennaID', all.x = TRUE, sort = TRUE)
  rm(y1)
  colnames(y2)<-c('antennaID2', 'antennaID1', 'WKT1', 'WKT2')
  res<-vector(length = nrow(y2))
  for(i in 1:nrow(y2)) {
    p1<-y2[[i,'WKT1']]
    p2<-y2[[i,'WKT2']]
    res[i]<-gIntersects(p1,p2)
  }
  y2[,'neighbour']<-res
  return (y2[,.(antennaID1, antennaID2, neighbour)])
}
