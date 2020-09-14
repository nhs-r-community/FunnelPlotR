#' @title Funnel Plots drawing function
#' @description Internal function for drawing plot.  Do not use this directly, call `funnel_plot()` instead.
#'
#' @param mod_plot_agg data.frame of containing numerator, denominator, ratio/proportion, SEs and limits
#' @param limits data.frame of limits from set_plot_range().
#' @param x_label Title for the funnel plot x-axis.  Usually expected deaths, readmissions, incidents etc.
#' @param y_label Title for the funnel plot y-axis.  Usually a standardised ratio.
#' @param title Plot title
#' @param label_outliers Add group labels to outliers on plot. Accepted values are\: 95 or 99 corresponding to 95\% or 99.8\% quantiles of the distribution. Default=99
#' @param multiplier Scale relative risk and funnel by this factor. Default to 1, but 100 is used for HSMR
#' @param Poisson_limits Draw exact limits based only on data points with no iterpolation. (default=FALSE)
#' @param OD_adjust Draw overdispersed limits using Spiegelhalter's (2012) tau2 (default=TRUE)
#' @param target the calculated target value for the data type
#' @param min_y Specify the plot range. 
#' @param max_y Specify the plot range. 
#' @param min_x Specify the plot range. 
#' @param max_x Specify the plot range. 
#' @param data_type the data type SR, PR or RC.
#' @param sr_method CQC or SHMI methods for standardised ratios
#' @param theme a ggplot theme function.
#'
#' @return A list containing [1] the funnel plot as a ggplot2 object., [2]the limits table.
#' @keywords internal
#'
#'
#' @importFrom scales comma
#' @importFrom ggrepel geom_label_repel
#' @import ggplot2


draw_plot<-function(mod_plot_agg, limits, x_label, y_label, title, label_outliers, multiplier,
                    Poisson_limits, OD_adjust, target, min_y, max_y, min_x, max_x, data_type, 
                    sr_method, theme, plot_cols){
  
  # Bind variable for NSE
  numerator <- denominator <- number.seq <- ll95 <- ul95 <- ll998 <- ul998 <- odll95 <- odul95 <-
    odll998 <- odul998 <- rr <- UCL95 <- group <- LCL95 <- OD95UCL <- OD95LCL <-UCL99 <-LCL99 <-
    OD99UCL <- OD99LCL <- outlier <- NULL
  

  # base funnel plot
  funnel_p <- ggplot(mod_plot_agg, aes(y = multiplier * ((numerator / denominator)), x = denominator)) +
    geom_point(size = 2, alpha = 0.55, shape = 21, fill = "dodgerblue") +
    geom_hline(aes(yintercept = target), linetype = 2) +
    xlab(x_label) +
    ylab(y_label) +
    ggtitle(title) +
    theme +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, face = "italic")
      ) +
    guides(colour = guide_legend(title.theme = element_text(
      size = 10,
      face = "bold",
      colour = "black",
      angle = 0
    )))




  if (Poisson_limits == TRUE & OD_adjust == TRUE) {
    funnel_p <- funnel_p +
      geom_line(aes(x = number.seq, y = ll95, col = "95% Poisson"), size = 1, linetype = 2, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = ul95, col = "95% Poisson"), size = 1, linetype = 2, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = ll998, col = "99.8% Poisson"), size = 1, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = ul998, col = "99.8% Poisson"), size = 1, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = odll95, col = "95% Overdispersed"), size = 1, linetype = 2, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = odul95, col = "95% Overdispersed"), size = 1, linetype = 2, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = odll998, col = "99.8% Overdispersed"), size = 1, data = limits, na.rm = TRUE) +
      geom_line(aes(x = number.seq, y = odul998, col = "99.8% Overdispersed"), size = 1, data = limits, na.rm = TRUE) +
      scale_color_manual(values = plot_cols, name = "Control limits")
  } else {
    if (Poisson_limits == TRUE & OD_adjust == FALSE) {
      funnel_p <- funnel_p +
        geom_line(aes(x = number.seq, y = ll95, col = "95% Poisson"), size = 1, linetype = 2, data = limits, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = ul95, col = "95% Poisson"), size = 1, linetype = 2, data = limits, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = ll998, col = "99.8% Poisson"), size = 1, data = limits, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = ul998, col = "99.8% Poisson"), size = 1, data = limits, na.rm = TRUE) +
        scale_color_manual(values = plot_cols[1:2], name = "Control limits")
    }

    if (Poisson_limits == FALSE &  OD_adjust == TRUE) {
      funnel_p <- funnel_p +
        geom_line(aes(x = number.seq, y = odll95, col = "95% Overdispersed"), size = 1, linetype = 2, data = limits, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = odul95, col = "95% Overdispersed"), size = 1, linetype = 2, data = limits, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = odll998, col = "99.8% Overdispersed"), size = 1, data = limits, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = odul998, col = "99.8% Overdispersed"), size = 1, data = limits, na.rm = TRUE) +
        scale_color_manual(values = plot_cols[3:4], name = "Control limits")
    }
  }

  # Apply plot scaling
  if (OD_adjust == TRUE) {
    funnel_p <- funnel_p +
      scale_y_continuous(name = y_label, limits = c(min_y, max_y))+
      scale_x_continuous(name = x_label, labels = scales::comma, limits = c(min_x -1, max_x + 1))
  } else {
    funnel_p <- funnel_p +
      scale_y_continuous(name = y_label, limits=c(min_y, max_y))+
      scale_x_continuous(name = x_label, labels = scales::comma, limits = c(min_x -1, max_x + 1))
  }

  
  
  

 # Label outliers
  
  if(label_outliers==TRUE){
  
    funnel_p <- funnel_p +
      geom_label_repel(aes(label = ifelse(outlier == 1,
                                          as.character(group), "")), size = 2.7, direction = "y",
                       force = 2, min.segment.length = 0)
  }
     
  
  # if (label_outliers == 95) {
  #   if (OD_adjust==FALSE) {
  #     funnel_p <- funnel_p +
  #       geom_label_repel(aes(label = ifelse(rr > UCL95,
  #                                            as.character(group), "")), size = 2.7, direction = "y",
  #                                 force = 2, min.segment.length = 0) +
  #       geom_label_repel(aes(label = ifelse(rr < LCL95,
  #                                           as.character(group), "")), size = 2.7, direction = "y",
  #                                 force = 2, min.segment.length = 0)
  #   } else {
  #     funnel_p <- funnel_p +
  #       geom_label_repel(aes(label = ifelse(rr > OD95UCL,
  #                                           as.character(group), "")), size = 2.7, direction = "y",
  #                                 force = 2, min.segment.length = 0) +
  #       geom_label_repel(aes(label = ifelse(rr < OD95LCL,
  #                                           as.character(group), "")), size = 2.7, direction = "y",
  #                                 force = 2, min.segment.length = 0)
  #   }
  # }
  # if (label_outliers == 99) {
  #   if (OD_adjust==FALSE) {
  #     funnel_p <- funnel_p +
  #       geom_label_repel(aes(label = ifelse(rr > UCL99,
  #                                              as.character(group), "")), size = 2.7, direction = "y",
  #                                 force = 2, min.segment.length = 0) +
  #       geom_label_repel(aes(label = ifelse(rr < LCL99,
  #                                              as.character(group), "")), size = 2.7, direction = "y",
  #                                 force = 2, min.segment.length = 0)
  #   } else {
  #     funnel_p <- funnel_p +
  #      geom_label_repel(aes(label = ifelse(rr > OD99UCL,
  #                                              as.character(group), "")), size = 2.7, direction = "y",
  #                                 force = 2, min.segment.length = 0) +
  #      geom_label_repel(aes(label = ifelse(rr < OD99LCL,
  #                                             as.character(group), "")), size = 2.7, direction = "y",
  #                                 force = 2, min.segment.length = 0)
  #   }
  # }

 return(funnel_p)

}
