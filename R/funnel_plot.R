#' @title Funnel Plots for Indirectly-Standardised Ratios
#' @description This is an implementation of funnel plots described Spiegelhalter (2005).
#' There are several parameters for the input, with the assumption that you will want smooth,
#'  overdispersed, funnel limits plotted based on the DerSimmonian Laird \eqn{\tau^2} additive random
#' effects models.
#'
#' @param numerator  A vector of the numerator (observed events/counts) values.  Used as numerator of the Y-axis
#' @param denominator A vector of denominator (predicted/population etc).  Used as denominator of the Y-axis and the scale of the x-axis
#' @param group A vector of group names or a factor.  Used to aggreagate and group points on plots
#' @param title Plot title
#' @param label_outliers Add group labels to outliers on plot. Accepted values are: 95 or 99 corresponding to 95\% or 99.8\% quantiles of the distribution. Default=99
#' @param Poisson_limits Draw exact limits based only on data points with no iterpolation. (default=FALSE)
#' @param OD_adjust Draw overdispersed limits using Speigelhalter's (2012) Tau2 (default=TRUE)
#' @param method Either "CQC" or "SHMI" (default). There are a few methods for standardisation.  CQC/Spiegelhalter
#' uses a square-root transformation and Winsorises by replacing values, SHMI uses log-transformation and doesn't Winsorise as such, but
#' truncates the distribution instead.  Both methods as used before calcuating the dispersion ratio (phi), and plot is drawn for the full dataset, not the
#' Winsorised/truncated one. SHMI method is default.
#' @param Winsorise_by Proportion of the distribution for winsorization. Default is 10 \% (0.1)
#' @param multiplier Scale relative risk and funnel by this factor. Default to 1, but 100 is used for HSMR
#' @param x_label Title for the funnel plot x-axis.  Usually expected deaths, readmissions, incidents etc.
#' @param y_label Title for the funnel plot y-axis.  Usually a standardised ratio.
#' @param aggregate_input_data Should the function aggreagate the inputs, by group, before passing into OD adjustment and plot? Default is TRUE.
#' @param yrange Manually specify the y-axis min and max, in form c(min, max), e.g. c(0.7, 1.3). Default, NULL, allows function to estimate range.
#' @param xrange Manually specify the y-axis min and max, in form c(min, max), e.g. c(0, 200). Default, NULL, allows function to estimate range.
#' @param return_elements a vector of elements ot return, options include "plot" for ggplot2 object, "data" for data after processing, and "limits" for control limit lookup table used in the plot. Default is all three objects
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
#' \donotrun{
#' # lets use the \'medpar\' dataset from the \'COUNT\' package.
#' # Little reformatting needed
#'
#' library(COUNT)
#' data(medpar)
#' medpar$provnum<-factor(medpar$provnum)
#' medpar$los<-as.numeric(medpar$los)
#'
#' mod<- glm(los ~ hmo + died + age80 + factor(type)
#'       , family="poisson", data=medpar)
#' summary(mod)
#'
#' # Get predicted value for ratio
#' medpar$prds<- predict(mod, type="response")
#'
#' # Draw plot, returning just the plot object
#' fp<-funnel_plot_dev(denominator=medpar$prds,numerator=medpar$los,
#' group = medpar$provnum, return_elements=c("plot"))
#'fp
#'}
#'
#' @seealso \href{https://rss.onlinelibrary.wiley.com/doi/full/10.1111/j.1467-985X.2011.01010.x}{Statistical methods for healthcare regulation: rating, screening and surveillance. Spiegelhalter et al (2012)}
#'    \href{https://onlinelibrary.wiley.com/doi/10.1002/sim.1970}{Funnel plots for comparing institutional performance. Spiegelhalter (2004)}
#'    \href{https://qualitysafety.bmj.com/content/14/5/347}{Handeling over-dispersion of performance indicators. Spiegelhalter (2005)}
#'
#' @importFrom scales comma
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr select filter arrange mutate summarise group_by %>% n
#' @import ggplot2


funnel_plot <- function(numerator, denominator, group, aggregate_input_data=TRUE, label_outliers = 99,
                            Poisson_limits = FALSE, OD_adjust = TRUE, method = "SHMI", Winsorise_by = 0.1,
                            title="Untitled Funnel Plot", multiplier = 1, x_label = "Expected",
                            y_label = "Standardised Ratio", yrange, xrange, return_elements=c("plot", "data", "limits")){


#funnel_plot(medpar$los, medpar$prds, medpar$provnum)

  # build initial dataframe of obs/predicted, with error message caught here in 'try'

  if (missing(denominator)) {
    stop("Need to specify model denominator")
  }
  if (missing(numerator)) {
    stop("Need to supply numerator")
  }
  if (missing(title)) {
    title <- ("Untitled Funnel Plot")
  }
  if (missing(numerator)) {
    stop("Need to supply the column name for numerator")
  }

  if (class(denominator)[1] == "array") {
    denominator <- as.numeric(denominator)
  }


  mod_plot <- data.frame(numerator, denominator, group)

  if (aggregate_input_data==TRUE){

    mod_plot_agg<-aggregate_func(mod_plot)



  } else {
    mod_plot_agg <- mod_plot
    mod_plot_agg$rr <- numerator / denominator

  }

 #OD Adjust and return table
  adj<-OD_adjust_func(mod_plot_agg, method=method, Winsorise_by= Winsorise_by, bypass=FALSE)
  mod_plot_agg<-as.data.frame(adj[1])

 # Need to sort out bypass mechanism, but set phi and tau to zero as needed
  if(OD_adjust == TRUE){
  phi<-as.numeric(adj[2])
  Tau2<-as.numeric(adj[3])
  } else {
  phi<-as.numeric(0)
  Tau2<-as.numeric(0)
  }

  fun_plot<-draw_plot(mod_plot_agg, yrange, xrange, x_label, y_label, title, label_outliers,
                      multiplier=multiplier, Poisson_limits, OD_Tau2=OD_adjust, Tau2=Tau2, method=method)

  #Build return
  rtn<-list()
  if(length(grep("plot", return_elements))>0){
  rtn[["plot"]]<-fun_plot[[1]]
  }
  if(length(grep("data", return_elements))>0){
  rtn[["data"]]<-mod_plot_agg
  }
  if(length(grep("limits", return_elements))>0){
  rtn[["limits"]]<-fun_plot[[2]]
  }

  return(rtn)
}
