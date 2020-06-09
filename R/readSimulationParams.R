
#' @import xml2
#' @export
readSimulationParams <- function(simFileName) {
  
  simulation.xml  <- as_list(read_xml(simFileName))
  simulation.xml<-simulation.xml$simulation
  simulation.xml$start_time <- as.numeric(simulation.xml$start_time)
  simulation.xml$end_time <- as.numeric(simulation.xml$end_time)
  simulation.xml$time_increment <- as.numeric(simulation.xml$time_increment)
  simulation.xml$time_stay <- as.numeric(simulation.xml$time_stay)
  simulation.xml$interval_between_stays <- as.numeric(simulation.xml$interval_between_stays)
  simulation.xml$prob_sec_mobile_phone <- as.numeric(simulation.xml$prob_sec_mobile_phone)
  simulation.xml$conn_threshold <- as.numeric(simulation.xml$conn_threshold)
  return (simulation.xml)

}