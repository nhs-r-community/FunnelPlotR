#' @description This is a basic implementation of Speigelhalter's funnel plots for using wiht Chris Mainey's
#' PhD work, using aggregated incident reporting data from NRLS.  It expects a specific structure
#' of table and column names.
#'
#' @author Chris Mainey
#' @docType package
#' @name CMFunnels
#' @title Funnel Plots of Incident reports
NULL


require(lme4)
require(splines)
require(mgcv)
require(ggplot2)
require(scales)
require(MASS)
require(dplyr)
