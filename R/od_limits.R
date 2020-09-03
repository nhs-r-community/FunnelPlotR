#' Overdispersion-adjusted funnel limit calculation
#'
#' @description Add 95% and 99.8 % funnel limits from OD-adjusted Poisson distribution
#'
#' @param mod_plot_agg Aggregated model input data
#' @param data_type Type of data for adjustment and plotting: Indirectly Standardised ratio (\"SR\"), proportion (\"PR\"), or ratio of counts (\"RC\").
#' @param sr_method Adjustment method for standardised ratios, can take the value \"SHMI\" or \"CQC\". \"SHMI\" is default. 
#' @param multiplier Multiplier to adjust limits if reporting by a multiplier, e.g. per 1000.
#' @param Tau2 A 'between' standard deviation to add to the within standard deviation, S, to inflate limits.
#' @param Target The centre line of the plot. Mean for non-SRs or 1 for SR
#'
#' @return A data.frame of with appended OD limits
#' @export
#' 
OD_limits<-function(mod_plot_agg=mod_plot_agg, data_type = "SR", sr_method = "SHMI", multiplier = 1, Tau2 = 0
                    ,Target=Target){
  
  if(data_type == "SR"){
    
    if(sr_method == "SHMI"){
      mod_plot_agg$OD95LCL <- multiplier * (exp(-1.959964 * sqrt((1/mod_plot_agg$denominator) + Tau2)))
      mod_plot_agg$OD95UCL <- multiplier * (exp(1.959964 * sqrt((1/mod_plot_agg$denominator) + Tau2)))
      mod_plot_agg$OD99LCL <- multiplier * (exp(-3.090232 * sqrt((1/mod_plot_agg$denominator) + Tau2)))
      mod_plot_agg$OD99UCL <- multiplier * (exp(3.090232 * sqrt((1/mod_plot_agg$denominator) + Tau2)))
      
    } else { 
      
      mod_plot_agg$OD95LCL <- multiplier * (mod_plot_agg$target_transformed - (1.959964 * sqrt( (mod_plot_agg$s + sqrt(tau2)^2))))
      mod_plot_agg$OD95UCL <- multiplier * (mod_plot_agg$target_transformed + (1.959964 * sqrt( (mod_plot_agg$s + sqrt(tau2)^2))))
      mod_plot_agg$OD99LCL <- multiplier * (mod_plot_agg$target_transformed - (3.090232 * sqrt( (mod_plot_agg$s + sqrt(tau2)^2))))
      mod_plot_agg$OD99UCL <- multiplier * (mod_plot_agg$target_transformed + (3.090232 * sqrt( (mod_plot_agg$s + sqrt(tau2)^2))))
      
    }
    
  } else if(data_type=="RC"){
    
    
    mod_plot_agg$OD95LCL <- multiplier * (exp( mod_plot_agg$Target_transformed - (1.959964 * sqrt( mod_plot_agg$s^2 + Tau2))))
    mod_plot_agg$OD95UCL <- multiplier * (exp( mod_plot_agg$Target_transformed + (1.959964 * sqrt( mod_plot_agg$s^2 + Tau2))))
    mod_plot_agg$OD99LCL <- multiplier * (exp( mod_plot_agg$Target_transformed - (3.090232 * sqrt( mod_plot_agg$s^2 + Tau2))))
    mod_plot_agg$OD99UCL <- multiplier * (exp( mod_plot_agg$Target_transformed + (3.090232 * sqrt( mod_plot_agg$s^2 + Tau2))))
    
  } else if(data_type=="PR"){
    
    mod_plot_agg$OD95LCL <-  multiplier * sin(mod_plot_agg$Target_transformed - 1.959964 * sqrt((mod_plot_agg$s^2) +Tau2))^2
    mod_plot_agg$OD95UCL <-  multiplier * sin(mod_plot_agg$Target_transformed + 1.959964 * sqrt((mod_plot_agg$s^2) +Tau2))^2
    mod_plot_agg$OD99LCL <-  multiplier * sin(mod_plot_agg$Target_transformed - 3.090232 * sqrt((mod_plot_agg$s^2) +Tau2))^2
    mod_plot_agg$OD99UCL <-  multiplier * sin(mod_plot_agg$Target_transformed + 3.090232 * sqrt((mod_plot_agg$s^2) +Tau2))^2
    
  }
  
  return(mod_plot_agg)
}

