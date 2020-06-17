
#' @title 
#' @description 
#' @param 
#' @param 
#' @return 
#' @import data.table
#' @import destim
#' @import parallel
#' @import doParallel
#' 
buildCluster <- function(varlist, env) {
  
  if (Sys.info()[['sysname']] == 'Linux' |
      Sys.info()[['sysname']] == 'Darwin') {
    cl <- makeCluster(detectCores(), type = "FORK")
  } else {
    cl <- makeCluster(detectCores())
    clusterEvalQ(cl, library("destim"))
    clusterEvalQ(cl, library("data.table"))
    clusterExport(cl, varlist, envir = env)
  }
  return (cl)
}