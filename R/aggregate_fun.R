#' @title Funnel Plots for Indirectly-Standardised Ratios
#' @description This is an implementation of funnel plots described Spiegelhalter (2005).
#' There are several parameters for the input, with the assumption that you will want smooth,
#'  overdispersed, funnel limits plotted based on the DerSimmonian Laird \eqn{\tau^2} additive random
#' effects models.
#'
#' @param predictions A vector of model predictions.  Used as denominator of the Y-axis and the scale of the x-axis
#' @param observed  A vector of the observed value.  Used as numerator of the Y-axis
#' @param group A vector of group names or a factor.  Used to aggreagate and group points on plots

aggreagate_func<- function(predictions, observed, group) {
  
  mod_plot <- data.frame(preds = predictions, obs = observed, grp = group)
  
  mod_plot_agg <- mod_plot %>%
    dplyr::group_by(grp) %>%
    dplyr::summarise(
      observed = as.numeric(sum(obs)),
      predicted = as.numeric(sum(preds)),
      rr = observed / predicted
    )

}