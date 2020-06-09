
#' @import data.table
#' @import rgeos
#' @export
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


