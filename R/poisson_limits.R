#' Poisson funnel limit calculation
#'
#' @description Adds 95% and 99.8 % funnel limits from Poisson distribution, using exact Poisson limits.
#'
#' @param mod_plot_agg Aggregated model input data
#' @param multiplier Multiplier to adjust limits if reporting by a multiplier, e.g. per 1000.
#' @param target supplied target value
#' 
#' @return A data.frame of original, aggregated data plus transformed z-score (unadjusted for overdispersion)
#' @keywords internal
#' 
#' @importFrom stats qchisq
#' 
poisson_limits<-function(mod_plot_agg=mod_plot_agg, multiplier = 1, target = target){
  
  mod_plot_agg$LCL95 <- multiplier * target * (qchisq(0.975, (2*mod_plot_agg$denominator+1), lower.tail = FALSE)/2)/ mod_plot_agg$denominator
  mod_plot_agg$UCL95 <- multiplier * target * (qchisq(0.025, 2*(mod_plot_agg$denominator), lower.tail = FALSE)/2) / mod_plot_agg$denominator
  mod_plot_agg$LCL99 <- multiplier * target * (qchisq(0.999, (2*mod_plot_agg$denominator+1), lower.tail = FALSE)/2)/ mod_plot_agg$denominator
  mod_plot_agg$UCL99 <- multiplier * target * (qchisq(0.001, 2*(mod_plot_agg$denominator), lower.tail = FALSE)/2) / mod_plot_agg$denominator
  
  return(mod_plot_agg)
}