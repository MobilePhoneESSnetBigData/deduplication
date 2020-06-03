






antennaNeighbours <- function(antennasFileName) {
  
  if(simulatedData == TRUE) {
    
    antenna.dt <- fread(antennasFileName, sep = ',', stringsAsFactors = FALSE, header = TRUE,
                       colClasses = c('integer', 'character', 'numeric', 'numeric', 'integer', 'integer'))
  
  # Obtain distances between each pair of antennas
  D <- data.table(as.matrix(dist(antenna.dt[, .(x,y)])))
  setnames(D, antenna.dt[, antennaID])
  D[, antennaID1 := antenna.dt[, antennaID]]
  D2 <- melt(D, id.vars = c("antennaID1"),
             variable.name = "antennaID2", value.name = "dist",
             variable.factor = FALSE, na.rm = TRUE)
  
  # Join the information about the cover radio of each antenna
  D3 <- merge(D2, antennaQualitySignalParam.dt[, .(antennaID, cover_radio)],
              by.x = "antennaID1", by.y = "antennaID", all.x = TRUE)
  setnames(D3, "cover_radio", "cover_radio1")
  D4 <- merge(D3, antennaQualitySignalParam.dt[, .(antennaID, cover_radio)],
              by.x = "antennaID2", by.y = "antennaID", all.x = TRUE)
  setnames(D4, old = "cover_radio", new = "cover_radio2")
  D4[, signal_dist := cover_radio1 + cover_radio2]
  
  # Consider neighbours antennas the ones with the signal overlapped
  antenna_neig <- D4[signal_dist > dist][, nei := paste0(antennaID1, "-", antennaID2)]
  # de aici trebuie taiate perechile id-id (aceeasi antena)
  
  
}