#' @title Funnel Plots for Indirectly-Standardised Ratios
#' @description An implementation of funnel plots for indirectly standardised ratios, as described by Spiegelhalter (2005) <doi:10.1002/sim.1970>.
#' There are several parameters for the input, with the assumption that you will want smooth,
#'  overdispersed, funnel control limits.  Limits may be inflated for overdispersion based on the DerSimmonian Laird \eqn{\tau^2} additive random
#' effects models, originally described for meta-analysis.
#'
#' @param numerator  A vector of the numerator (observed events/counts) values.  Used as numerator of the Y-axis
#' @param denominator A vector of denominator (predicted/population etc).  Used as denominator of the Y-axis and the scale of the x-axis
#' @param group A vector of group names as character or factor.  Used to aggregate and group points on plots
#' @param data_type A string identifying the type of data used for in the plot, the adjustment used and the reference point. One of: "SR" forindirectly standardised ratios, such SHMI, "PR" for proportions, or "RC" for ratios of counts. Default is "SR".
#' @param title Plot title
#' @param label_outliers Add group labels to outliers on plot. Accepted values are: 95 or 99 corresponding to 95\% or 99.8\% quantiles of the distribution. Default=99
#' @param Poisson_limits Draw exact Poisson limits, without overdispersion adjustment. (default=FALSE)
#' @param OD_adjust Draw overdispersed limits using hierarchical model, assuming at group level, as described in Spiegelhalter (2012) <doi:https://doi.org/10.1111/j.1467-985X.2011.01010.x>.
#' It calculates a second variance component ' for the 'between' standard deviation (Tau2), that is added to the 'within' standard deviation (sigma) (default=TRUE)
#' @param sr_method Method for adjustment when using indirectly standardised ratios (type="SR") Either "CQC" or "SHMI" (default). There are a few methods for standardisation.  "CQC"/Spiegelhalter
#' uses a square-root transformation and Winsorises (rescales the outer most values to a particular percentile).
#' SHMI, instead, uses log-transformation and doesn't Winsorise, but truncates the distribution before assessing overdisperison.
#' Both methods then calculate a dispersion ratio (phi) on this altered dataset.  This ratio is then used to scale the full dataset,
#' and the plot is drawn for the full dataset.
#' @param Winsorise_by Proportion of the distribution for winsorisation/truncation. Default is 10 \% (0.1).  Note, this is applied in a two-sided
#' fashion, e.g. 10\% refers to 10\% at each end of the distribution (20\% winsorised/truncated)
#' @param multiplier Scale relative risk and funnel by this factor. Default to 1, but 100 sometime used, e.g. in some hospital mortality ratios.
#' @param x_label Title for the funnel plot x-axis.  Usually expected deaths, readmissions, incidents etc.
#' @param y_label Title for the funnel plot y-axis.  Usually a standardised ratio.
#' @param xrange Manually specify the y-axis min and max, in form c(min, max), e.g. c(0, 200). Default, "auto", allows function to estimate range.
#' @param yrange Manually specify the y-axis min and max, in form c(min, max), e.g. c(0.7, 1.3). Default, "auto", allows function to estimate range.
#' @param return_elements a vector of elements to return, options include "plot" for ggplot2 object, "data" for data after processing, and "limits" for control
#' limit lookup table used in the plot. Default is all three objects.
#' @param theme a ggplot theme function.  This can be a canned theme such as theme_bw(), a theme() with arguments, or your own custom theme function. Default is new funnel_clean(), but funnel_classic() is original format.
#'
#' @return A list containing [1] the funnel plot as a ggplot2 object, [2] the base table for the plot, [3] the limits table.
#'
#' @export
#' @details
#'    Outliers are marked based on the grouping, and controlled by `label_outliers` .
#'    Overdispersion can be factored in based on the methods in Spiegelhalter et al (2012) <doi:https://doi.org/10.1111/j.1467-985X.2011.01010.x>, set `OD_adjust` to FALSE to suppress this. \cr
#'    To use Poisson limits set `Poisson_limits=TRUE`. This uses 95% & 99.8% limits limits. \cr
#'    It deliberately avoids red-amber-green colouring, but you could extract this from the ggplot object and change manually if you like.
#' @examples
#' #' # We will use the 'medpar' dataset from the 'COUNT' package.
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
#' # Get predicted values for building ratio
#' medpar$prds<- predict(mod, type="response")
#'
#' # Draw plot, returning just the plot object
#' fp<-funnel_plot(denominator=medpar$prds,numerator=medpar$los,
#' group = medpar$provnum, return_elements=c("plot"))
#'fp
#'
#'
#' @seealso Statistical methods for healthcare regulation: rating, screening and surveillance. Spiegelhalter et al (2012) <doi:https://doi.org/10.1111/j.1467-985X.2011.01010.x> \cr
#' Funnel plots for comparing institutional performance. Spiegelhalter (2005) <doi:10.1002/sim.1970> \cr
#' Handling over-dispersion of performance indicators. Spiegelhalter (2005) <doi:10.1136/qshc.2005.013755>
#'
#' @importFrom scales comma
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr select filter arrange mutate summarise group_by %>% n
#' @import ggplot2


funnel_plot <- function(numerator, denominator, group, data_type = "SR", label_outliers = 99,
                            Poisson_limits = FALSE, OD_adjust = TRUE, sr_method = "SHMI", Winsorise_by = 0.1,
                            title="Untitled Funnel Plot", multiplier = 1, x_label = "Expected",
                            y_label = "Standardised Ratio",xrange = "auto", yrange = "auto",
                            return_elements=c("plot", "data", "limits"), theme = funnel_clean()){


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

  mod_plot_agg<-aggregate_func(mod_plot)

  
  if(OD_adjust == TRUE){
  #OD Adjust and return table
  # transform to z-score
  mod_plot_agg <- transformed_zscore(mod_plot_agg=mod_plot_agg, data_type = data_type, sr_method = sr_method)
  
  # Winsorise or truncate depeding on method
  mod_plot_agg <- winsorisation(mod_plot_agg = mod_plot_agg, sr_method = sr_method, Winsorise_by=Winsorise_by)
  
  n <- sum(!is.na(mod_plot_agg$Wuzscore))
  # Calcualte Phi (the overdiseprsion factor)
  phi <- phi_func(n= n, zscores=na.omit(mod_plot_agg$Wuzscore))
  
  # Use phi to calculate Tau, the between group standard deviation
  Tau2 <- tau_func(n=n,  phi=phi, mod_plot_agg$s)
  } else {
  
  phi<-as.numeric(0)
  Tau2<-as.numeric(0)
  }

  fun_plot<-draw_plot(mod_plot_agg, x_label, y_label, title, label_outliers,
                      multiplier=multiplier, Poisson_limits, OD_adjust=OD_adjust,
                      Tau2=Tau2, xrange=xrange, yrange=yrange, theme = theme)

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
