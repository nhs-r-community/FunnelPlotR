#' @title Aggregation function for record-level data, prior to plot.
#' @description Internal funciton to aggreagate record-level data for plotting as a funnel.
#' @param mod_plot A data frame of values to be aggreagated.  Expected columns, 'group', 'numerator' and 'denominator'
#'
#' @return Returns an aggreagated data.frame of the same columns, summer by but 'group', with a relative risk 'rr' column added.
#' @importFrom dplyr group_by summarise %>%

aggregate_func<- function(mod_plot) {

  mod_plot_agg <- mod_plot %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      numerator = as.numeric(sum(numerator)),
      denominator = as.numeric(sum(denominator)),
      rr = numerator / denominator
    )

return(mod_plot_agg)

}
