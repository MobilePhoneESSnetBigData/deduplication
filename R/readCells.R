
#' @import data.table
#' @import rgeos
#' 
# readCells <- function (cellsFileName) {
#   con = file(cellsFileName, "r")
#   while ( TRUE ) {
#     line = readLines(con, n = 1)
#     if ( length(line) == 0 ) {
#       break
#     }
#     str = str_split_fixed(line, pattern = ',', n = 2)
#     antennaId = str[1,1]
#     cell <- str[1,2]
#     print(antennaId)
#   }
#   close(con)
# }
#file<-'/home/bogdan/r-projects/vis-david/dataset5/AntennaCells_Vodafone.csv'
#file<-'/home/bogdan/r-projects/vis-david2/dataset1b/AntennaCells_Vodafone.csv'


readCells <- function (cellsFileName) {
  coverArea <- fread(cellsFileName, sep ='\n', header = TRUE, stringsAsFactors = FALSE)
  setnames(coverArea, 'lines')
  coverArea[, antennaID := tstrsplit(lines, split = ',POLYGON')[[1]]]
  coverArea[, wkt := substring(lines, regexpr("POLYGON", lines))]
  coverArea <- coverArea[, c('antennaID', 'wkt'), with = FALSE]
  antennas <- coverArea[['antennaID']]
  coverArea[,'cell'] <- sapply(coverArea[['wkt']], function(wkt){
    polygon <- readWKT(wkt)
    return(polygon)
  })
  coverArea <-coverArea[,-2]
  
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


