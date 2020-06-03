
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


readCells <- function (cellsFileName, simulatedData = TRUE) {
  if( simulatedData) {
    coverArea <- fread(cellsFileName, sep ='\n', header = TRUE, stringsAsFactors = FALSE)
    setnames(coverArea, 'lines')
    coverArea[, antennaID := tstrsplit(lines, split = ',POLYGON')[[1]]]
    coverArea[, wkt := substring(lines, regexpr("POLYGON", lines))]
    coverArea <- coverArea[, c('antennaID', 'wkt'), with = FALSE]
    coverArea[,'cell'] <- sapply(coverArea[['wkt']], function(wkt){
      polygon <- readWKT(wkt)
      return(polygon)
    })
    return (coverArea[,-2])
  }
  else {
    cat("read real mobile network cell file not implemented yet!")
    return (NULL)
  }
}


