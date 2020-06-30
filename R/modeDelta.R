#' @title Returns the mode of delta distribution
#' 
#' @description Returns the mode of the deltaX or deltaY distribution
#' 
#' @param deltaDistribution a data.table object that could be deltaX or deltaY distribution. The table has two columns: delta and p.
#' 
#' @return the mode of the delta distribution
#' 
#' @import data.table
#' 
#' @export
modeDelta <-function(deltaDistribution) {
  
  return(deltaDistribution[which.max(deltaDistribution$p), delta])
  
}
