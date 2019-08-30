#' @title Aggregation function for record-level data, prior to plot.
#' @description Internal function to aggregate record-level data for plotting as a funnel.
#' @param mod_plot A data frame of values to be aggregated.  Expected columns, 'group', 'numerator' and 'denominator'
#'
#' @return Returns an aggregated data.frame of the same columns, but summed by 'group', with a relative risk 'rr' column added.
#' @importFrom dplyr group_by summarise %>%
#' @importFrom rlang .data

aggregate_func<- function(mod_plot) {

  mod_plot_agg <- mod_plot %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(
      numerator = as.numeric(sum(.data$numerator)),
      denominator = as.numeric(sum(.data$denominator)),
      rr = .data$numerator / .data$denominator
    )

return(mod_plot_agg)

}
