#' @description This is a basic implementation of Speigelhalter's funnel plots for using with Chris Mainey's
#' PhD work, using aggregated incident reporting data from NRLS.  It expects a specific structure
#' of table and column names.
#'
#' @author Chris Mainey
#' @docType package
#' @name CMFunnels
#' @title Funnel Plots of Incident reports

require(scales)
require(arm)
require(ggrepel)
require(ggplot2)
require(dplyr)
require(COUNT)
