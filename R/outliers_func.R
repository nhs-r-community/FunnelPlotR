#' @title Label outliers
#' 
#' @param mod_plot_agg Aggregated data set for plotting
#' @param OD_adjust Logical for drawing OD limits, takes precedence over Poisson for outliers
#' @param Poisson_limits Logical for drawing Poisson limits
#' @param limit which limit to use.  Currently 95 or 99.
#' 
#' 
outliers_func <- function(mod_plot_agg, OD_adjust, Poisson_limits, limit){

  if (limit == 95) {
    if (OD_adjust==FALSE){
      mod_plot_agg$outlier <- ifelse(mod_plot_agg$rr <= mod_plot_agg$UCL95 &
                                 mod_plot_agg$rr >= mod_plot_agg$LCL95,
                                 0,
                                 1)
    } else {
      mod_plot_agg$outlier <- ifelse(mod_plot_agg$rr <= mod_plot_agg$OD95UCL &
                                       mod_plot_agg$rr >= mod_plot_agg$OD95LCL,
                                     0,
                                     1)
    }
  }
  
  if (limit == 99) {
    if (OD_adjust==FALSE){
      mod_plot_agg$outlier <- ifelse(mod_plot_agg$rr <= mod_plot_agg$UCL99 &
                                       mod_plot_agg$rr >= mod_plot_agg$LCL99,
                                     0,
                                     1)
    } else {
      mod_plot_agg$outlier <- ifelse(mod_plot_agg$rr <= mod_plot_agg$OD99UCL &
                                       mod_plot_agg$rr >= mod_plot_agg$OD99LCL,
                                     0,
                                     1)
    }
  }
  return(mod_plot_agg)
}
  
  

      
     
      
