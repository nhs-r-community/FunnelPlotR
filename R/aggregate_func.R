#' @title Aggregation function for record-level data, prior to plot.
#' @description Internal function to aggregate record-level data for plotting as a funnel.
#' @param mod_plot A data frame of values to be aggregated.  Expected columns, 'group', 'numerator' and 'denominator'
#' @keywords internal
#' @return Returns an aggregated data.frame of the same columns, but summed by 'group', with a relative risk 'rr' column added.
#' @importFrom stats aggregate

aggregate_func<- function(mod_plot) {

 mod_plot_agg <- 
   data.frame(
     aggregate(list(numerator =mod_plot$numerator, 
                    denominator=mod_plot$denominator)
               ,  by=list(group = mod_plot$group)
               , FUN=sum)
   )
   
  mod_plot_agg$rr = mod_plot_agg$numerator/ mod_plot_agg$denominator

return(mod_plot_agg)

}
