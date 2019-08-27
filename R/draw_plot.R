#' @title Funnel Plots drawing function
#' @description This function sets up the plots, and draws them with ggplot2.
#'
#' @param input_frame data frame containing numerator, denominator, ratio/proportion, SEs and limits
#' @param title Plot title
#' @param label_outliers Add group labels to outliers on plot. Accepted values are: 95 or 99 corresponding to 95\% or 99.8\% quantiles of the distribution. Default=99
#' @param Poisson_limits Draw exact limits based only on data points with no iterpolation. (default=FALSE)
#' @param OD_Tau2 Draw overdispersed limits using Speigelhalter's (2012) Tau2 (default=TRUE)
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
#' a <- funnel_plot(my_preds, my_numerator, "organisation", "2015/16", "Poisson model")
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
#' @import ggplot2

draw_plot<-function(mod_plot_agg, yrange=NULL, xrange=NULL, x_label, y_label, title,
                    label_outliers, multiplier, Poisson_limits, OD_Tau2, Tau2 = 0, method){

#plot ranges
  # Determine the range of plots
  max_preds <- dplyr::summarise(mod_plot_agg, ceiling(max(denominator))) %>% as.numeric()
  min_preds <- dplyr::summarise(mod_plot_agg, ceiling(min(denominator))) %>% as.numeric()
  min_ratio <- min(0.7 * multiplier, dplyr::summarise(mod_plot_agg, multiplier * min(numerator / denominator)) %>% as.numeric())
  max_ratio <- max(1.3 * multiplier, dplyr::summarise(mod_plot_agg, multiplier * max(numerator / denominator)) %>% as.numeric())


  ### Calculate funnel limits ####
  if (OD_Tau2 == FALSE) {
    Poisson_limits <- TRUE
    message("OD_adjust set to FALSE, plotting using Poisson limits")
  }

  if (OD_Tau2 == TRUE & Tau2 == 0) {
    OD_Tau2 <- FALSE
    Poisson_limits <- TRUE

    message("No overdispersion detected, or OD_adjust to FALSE, plotting using Poisson limits")

  }
  # if (OD_Tau2 == TRUE & Tau2 == 0) {
  #   OD_Tau2 <- FALSE
  #   Poisson_limits <- TRUE
  #
  #   )
  #
  # } else {
  #   stop("Invalid method supplied")
  # }

  dfCI<-build_limits_lookup(max_preds, min_preds, min_ratio, max_ratio, Poisson_limits, OD_Tau2, Tau2, method, multiplier)


  # base funnel plot
  funnel_p <- ggplot(mod_plot_agg, aes(y = multiplier * ((numerator / denominator)), x = denominator)) +
    geom_point(size = 2, alpha = 0.55, shape = 21, fill = "dodgerblue2") +
    # scale_y_continuous(limits = c((min_ratio-0.1), (max_ratio+0.1)))+
    # scale_x_continuous(labels = scales::comma, limits = c(0,max_preds+1)) +
    geom_hline(aes(yintercept = multiplier), linetype = 2) +
    xlab(x_label) +
    ylab(y_label) +
    ggtitle(title) +
    # theme_bw()+
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "italic")
      # plot.background =  element_rect(fill='white', colour='white'),
    ) +
    guides(colour = guide_legend(title.theme = element_text(
      size = 10,
      face = "bold",
      colour = "black",
      angle = 0
    )))




  if (Poisson_limits == TRUE & OD_Tau2 == TRUE) {
    funnel_p <- funnel_p +
      geom_line(aes(x = number.seq, y = ll95, col = "95% Poisson"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = ul95, col = "95% Poisson"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = ll998, col = "99.8% Poisson"), size = 1, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = ul998, col = "99.8% Poisson"), size = 1, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = odll95, col = "95% Overdispersed"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = odul95, col = "95% Overdispersed"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = odll998, col = "99.8% Overdispersed"), size = 1, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = odul998, col = "99.8% Overdispersed"), size = 1, data = dfCI, na.rm = TRUE) +
      scale_color_manual(values = c(
        "99.8% Poisson" = "#1F77B4FF",
        "95% Poisson" = "#FF7F0EFF",
        "99.8% Overdispersed" = "#2CA02CFF",
        "95% Overdispersed" = "#9467BDFF"
      ), name = "Control limits")
  } else {
    if (Poisson_limits == TRUE & OD_Tau2 == FALSE) {
      funnel_p <- funnel_p +
        geom_line(aes(x = number.seq, y = ll95, col = "95% Poisson"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = ul95, col = "95% Poisson"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = ll998, col = "99.8% Poisson"), size = 1, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = ul998, col = "99.8% Poisson"), size = 1, data = dfCI, na.rm = TRUE) +
        scale_color_manual(values = c(
          "99.8% Poisson" = "#7E9C06", # "#1F77B4FF"
          "95% Poisson" = "#FF7F0EFF"
        ), name = "Control limits")
    }

    if (Poisson_limits == FALSE &  OD_Tau2 == TRUE) {
      funnel_p <- funnel_p +
        geom_line(aes(x = number.seq, y = odll95, col = "95% Overdispersed"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = odul95, col = "95% Overdispersed"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = odll998, col = "99.8% Overdispersed"), size = 1, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = odul998, col = "99.8% Overdispersed"), size = 1, data = dfCI, na.rm = TRUE) +
        scale_color_manual(values = c(
          "99.8% Overdispersed" = "#DEC400", # "#F7EF0A", #"#2CA02CFF",
          "95% Overdispersed" = "#9467BDFF"
        ), name = "Control limits")
    }
  }


  if (OD_Tau2 == TRUE) {
    funnel_p <- funnel_p +
      scale_y_continuous(name = y_label, limits = c((multiplier * (min(min_ratio - 0.05, min(subset(mod_plot_agg, numerator>4)$OD99LCI) -0.1))), (multiplier * (max(max_ratio + 0.05, max(subset(mod_plot_agg, numerator>4)$OD99UCI) - 0.1))))) +
      scale_x_continuous(name = x_label, labels = scales::comma, limits = c(min_preds -1, max_preds + 1))
  } else {
    funnel_p <- funnel_p +
      scale_y_continuous(name = y_label, limits = c((multiplier * (min(min_ratio - 0.05, min(subset(mod_plot_agg, numerator>4)$LCL99) - 0.1))), (multiplier * (max(max_ratio + 0.05, max(subset(mod_plot_agg, numerator >4)$UCL99) + 0.1))))) +
      scale_x_continuous(name = x_label, labels = scales::comma, limits = c(min_preds -1, max_preds + 1))
  }


  if (label_outliers == 95) {
    if (OD_Tau2 == FALSE) {
      funnel_p <- funnel_p +
        ggrepel::geom_label_repel(aes(label = ifelse(numerator / denominator > UCL95, as.character(group), "")), size = 2.7, direction = "y") +
        ggrepel::geom_label_repel(aes(label = ifelse(numerator / denominator < LCL95, as.character(group), "")), size = 2.7, direction = "y")
    } else {
      funnel_p <- funnel_p +
        ggrepel::geom_label_repel(aes(label = ifelse(numerator / denominator > OD95UCI, as.character(group), "")), size = 2.7, direction = "y") +
        ggrepel::geom_label_repel(aes(label = ifelse(numerator / denominator < OD95LCI, as.character(group), "")), size = 2.7, direction = "y")
    }
  }
  if (label_outliers == 99) {
    if (OD_Tau2 == FALSE) {
      funnel_p <- funnel_p +
        ggrepel::geom_label_repel(aes(label = ifelse(numerator / denominator > UCL99, as.character(group), "")), size = 2.7, direction = "y") +
        ggrepel::geom_label_repel(aes(label = ifelse(numerator / denominator < LCL99, as.character(group), "")), size = 2.7, direction = "y")
    } else {
      funnel_p <- funnel_p +
        ggrepel::geom_label_repel(aes(label = ifelse(numerator / denominator > OD99UCI, as.character(group), "")), size = 2.7, direction = "y") +
        ggrepel::geom_label_repel(aes(label = ifelse(numerator / denominator < OD99LCI, as.character(group), "")), size = 2.7, direction = "y")
    }
  }

 return(funnel_p)

}
