

getEmmissionProbsJointModel <- function() {
  eventLoc_rasterCell.matrix <- cbind(eventLoc_rasterCell.matrix, '00' = rep(1, nrow(eventLoc_rasterCell.matrix)))
  new_eLoc_rCell.mt <- NULL
  for(j in 1:ncol(eventLoc_rasterCell.matrix)){
    cat(paste0(j, ", "))
    A <- eventLoc_rasterCell.matrix[, j] * eventLoc_rasterCell.matrix
    colnames(A) <- paste0(colnames(eventLoc_rasterCell.matrix)[j], "-", colnames(eventLoc_rasterCell.matrix))
    new_eLoc_rCell.mt <- cbind(new_eLoc_rCell.mt, A)
  }
  colToRem <- which(colnames(new_eLoc_rCell.mt) == "00-00")
  new_eLoc_rCell.mt <- new_eLoc_rCell.mt[, -colToRem]
  
  return(new_eLoc_rCell.mt)

}
