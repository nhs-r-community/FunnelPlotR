#' @title Funnel Plots for Indirectly-Standardised Ratios
#' @description This is an implementation of funnel plots described Spiegelhalter (2005).
#' There are several parameters for the input, with the assumption that you will want smooth,
#'  overdispersed, funnel limits plotted based on the DerSimmonian Laird \eqn{\tau^2} additive random
#' effects models.
#'
#' @param predictions A vector of model predictions.  Used as denominator of the Y-axis and the scale of the x-axis
#' @param observed  A vector of the observed value.  Used as numerator of the Y-axis
#' @param group A vector of group names or a factor.  Used to aggreagate and group points on plots
#' @param title Plot title
#' @param label_outliers Add group labels to outliers on plot. Accepted values are: 95 or 99 corresponding to 95\% or 99.8\% quantiles of the distribution. Default=99
#' @param Poisson_limits Draw exact limits based only on data points with no iterpolation. (default=FALSE)
#' @param OD_Tau2 Draw overdispersed limits using Speigelhalter's (2012) Tau2 (default=TRUE)
#' @param method Either "CQC" or "SHMI" (default). There are a few methods for standardisation.  CQC/Spiegelhalter
#' uses a square root transformation and winsorizes by replaceing values, SHMI uses log transformation and winsorizes
#' by truncation. SHMI method is default.
#' @param Winsorize_by Proportion of the distribution for winsorization. Default is 10 \% (0.1)
#' @param multiplier Scale relative risk and funnel by this factor. Default to 1, but 100 is used for HSMR
#' @param x_label Title for the funnel plot x-axis.  Usually expected deaths, readmissions, incidents etc.
#' @param y_label Title for the funnel plot y-axis.  Usually a standardised ratio.
#'
#' @return A list containing [1]the base table for the plot, [2]the limits table and [3]the funnel plot as a ggplot2 object.
#'
#' @export
#' @details
#'    Outliers are marked based on the grouping, controlled by `label_outliers` .
#'    Overdispersion can be factored in based on the methods in \href{https://rss.onlinelibrary.wiley.com/doi/full/10.1111/j.1467-985X.2011.01010.x}{Spiegelhalter et al (2012)}, set `OD_Tau2` to FALSE to suppress this.
#'    To use Poisson limits set `Poisson_limits=TRUE`. This uses 2 & 3 \eqn{\sigma} limits.
#'    It deliberatley avoids red-amber-green colouring, but you could extract this from the ggplot object and change manually if you like.
#'
#' @examples
#' \dontrun{
#' a <- funnel_plot(my_preds, my_observed, "organisation", "2015/16", "Poisson model")
#' # Access the plot
#' a[[3]]
#'
#' # Access the
#' }
#'
#' @seealso \href{https://rss.onlinelibrary.wiley.com/doi/full/10.1111/j.1467-985X.2011.01010.x}{Statistical methods for healthcare regulation: rating, screening and surveillance. Spiegelhalter et al (2012)}
#'    \href{https://onlinelibrary.wiley.com/doi/10.1002/sim.1970}{Funnel plots for comparing institutional performance. Spiegelhalter (2004)}
#'    \href{https://qualitysafety.bmj.com/content/14/5/347}{Handeling over-dispersion of performance indicators. Spiegelhalter (2005)}
#'
#' @importFrom scales comma
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr select filter arrange mutate summarise group_by %>% n
#' @importFrom stats predict qchisq quantile sd
#' @import ggplot2


funnel_plot_dev <- function(predictions, observed, group, title, aggregate_input=TRUE, label_outliers = 99,
                            Poisson_limits = FALSE, OD_Tau2 = TRUE, method = "SHMI", Winsorize_by = 0.1,
                            multiplier = 1, x_label = "Expected", y_label = "Standardised Ratio"){
  

  
  
  # build inital dataframe of obs/predicted, with error message caught here in 'try'
  
  if (missing(predictions)) {
    stop("Need to specify model predictions")
  }
  if (missing(observed)) {
    stop("Need to supply observed")
  }
  if (missing(title)) {
    title <- ("Untitled Funnel Plot")
  }
  if (missing(observed)) {
    stop("Need to supply the column name for observed events")
  }
  
  if (class(predictions)[1] == "array") {
    predictions <- as.numeric(predictions)
  }
  
  
  mod_plot <- data.frame(preds = predictions, obs = observed, grp = group)
  
  if (aggregate_input==TRUE){
    
    aggregate_fun(mod_plot)
    
  } else {
    mod_plot_agg <- mod_plot
    mod_plot_agg$rr <- observed / predicted
  }
  

  OD_adjust(mod_plot_agg, method=method, Winsorize_by= Winsorize_by)

   
  
  
}