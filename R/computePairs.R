


computePairs <- function(connections, ndevices, antennaNeighbors, P1, limit) {

  connections[is.na(connections)] <- "NA"
  connections <- data.table(connections)[, index := .I]
  
  # make combinations of all devices
  allPairs <- data.table(t(combn(c(1:ndevices), 2)))
  setnames(allPairs, c("index.x", "index.y"))
  
  allPairs_connections.dt1 <- merge(allPairs, connections , by.x = c("index.x"),
                                    by.y = c("index"), all.x = TRUE)
  allPairs_connections.dt2 <- merge(allPairs, connections , by.x = c("index.y"),
                                    by.y = c("index"), all.x = TRUE)
  setcolorder(allPairs_connections.dt2, names(allPairs_connections.dt1))
  allPairs_connections.dt <- rbindlist(list(allPairs_connections.dt1, allPairs_connections.dt2))
  
  #no longer needed
  rm(allPairs_connections.dt1)
  rm(allPairs_connections.dt2)
  
  allPairs_connections_concat.dt <- allPairs_connections.dt[, lapply(.SD, paste0, collapse="-"), by = c("index.x", "index.y")]
  
  allPairs_connections_concat.dt[allPairs_connections_concat.dt == "NA-NA"] <- NA
  
  keepCols <- names(allPairs_connections_concat.dt)[-which(names(allPairs_connections_concat.dt) %in% c("index.x", "index.y"))]
  
 
  number <- sapply(allPairs_connections_concat.dt[, ..keepCols],function(x){ x %in% antennaNeighbors[,nei]})
  
  allPairs_connections_concat.dt[, number := apply(number, 1, sum)]
  rm(number)
  
  allPairs_connections_concat.dt[, num_NA := Reduce(`+`, lapply(.SD,function(x) is.na(x))), .SDcols = keepCols]
  allPairs_connections_concat.dt[, final_num := number + num_NA]
  
  
  pairs4duplicity <- copy(allPairs_connections_concat.dt)[final_num >= quantile(c(0:length(keepCols)), probs = 1-P1-limit)][, c( "index.x", "index.y", keepCols), with = FALSE]
  rm(allPairs_connections_concat.dt)   
  return (pairs4duplicity)
}