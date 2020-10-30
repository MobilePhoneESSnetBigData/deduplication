#' deduplication: A package for computing the duplicity probability for mobile devices.

#' @title deduplication: A package for computing the duplicity probability for 
#' mobile device detected by the network.
#'
#' @description This package contains functions to compute the duplicity 
#' probability for mobile device detected by the network. It has three methods for
#' this purpose:  pairs, 1-to-1 and trajectory. The theory behind these 
#'methods is described in detail in \href{https://webgate.ec.europa.eu/fpfis/mwikis/essnetbigdata/images/f/fb/WPI_Deliverable_I3_A_proposed_production_framework_with_mobile_network_data_2020_05_31_draft.pdf}{WPI
#' Deliverable 3} and in the paper \emph{An end-to-end statistical process 
#' with mobile network data for Official Statistics}. For an example on how to 
#' use this package please read \link{example}.
#' @docType package
#' @name deduplication
#' @import Matrix
#' @import data.table
#' @import destim
#' @import doParallel
#' @import parallel
#' @import rgeos
#' @import stringr
#' @import xml2
#' @import XML
NULL