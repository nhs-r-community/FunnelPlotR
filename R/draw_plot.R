#' @title Funnel Plots drawing function
#' @description Internal function for drawing plot.  Do not use this directly, call `funnel_plot()` instead.
#'
#' @param mod_plot_agg data frame of containing numerator, denominator, ratio/proportion, SEs and limits
#' @param x_label Title for the funnel plot x-axis.  Usually expected deaths, readmissions, incidents etc.
#' @param y_label Title for the funnel plot y-axis.  Usually a standardised ratio.
#' @param title Plot title
#' @param label_outliers Add group labels to outliers on plot. Accepted values are\: 95 or 99 corresponding to 95\% or 99.8\% quantiles of the distribution. Default=99
#' @param multiplier Scale relative risk and funnel by this factor. Default to 1, but 100 is used for HSMR
#' @param Poisson_limits Draw exact limits based only on data points with no iterpolation. (default=FALSE)
#' @param OD_adjust Draw overdispersed limits using Spiegelhalter's (2012) Tau2 (default=TRUE)
#' @param Tau2 The Tau2 value to use for plotting limits
#' @param Target the calculated target value for the data type
#' @param yrange Specify the plot range. Default is "auto", else vector of length 2 e.g. c(0,200)
#' @param xrange Specify the plot range. Default is "auto", else vector of length 2 e.g. c(0,200)
#' @param data_type thedata type SR, PR or RC.
#' @param sr_method CQC or SHMI methods for standardised ratios
#' @param theme a ggplot theme function.
#'
#' @return A list containing [1] the funnel plot as a ggplot2 object., [2]the limits table.
#' @keywords internal
#'
#'
#' @importFrom scales comma
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang .data
#' @import ggplot2


draw_plot<-function(mod_plot_agg, x_label, y_label, title, label_outliers, multiplier,
                    Poisson_limits, OD_adjust, Tau2 = 0, Target, yrange, xrange, data_type, 
                    sr_method, theme){

#plot ranges
  # Determine the range of plots
  if(xrange[1] == "auto"){
    max_x <- as.numeric(ceiling(max(mod_plot_agg$denominator, na.rm = FALSE)))
    min_x <- as.numeric(ceiling(min(mod_plot_agg$denominator,na.rm = FALSE)))
  } else {
    min_x <- xrange[1]
    max_x <- xrange[2]
  }

  if(yrange[1] == "auto"){
    min_y <- min((0.7 * Target * multiplier), multiplier * (0.9 * as.numeric(min((mod_plot_agg$numerator / mod_plot_agg$denominator)))), na.rm = FALSE)

    max_y <- max((1.3 * Target *multiplier), multiplier *  (1.1 * as.numeric(max((mod_plot_agg$numerator / mod_plot_agg$denominator)))), na.rm = FALSE)
  } else {
    min_y <- yrange[1]
    max_y <- yrange[2]
  }

  ### Calculate funnel limits ####
  if (OD_adjust == FALSE) {
    Poisson_limits <- TRUE
    message("OD_adjust set to FALSE, plotting using Poisson limits")
  }

  if (OD_adjust == TRUE & Tau2 == 0) {
    OD_adjust <- FALSE
    Poisson_limits <- TRUE

    message("No overdispersion detected, or OD_adjust to FALSE, plotting using Poisson limits")

  }
  # if (OD_adjust == TRUE & Tau2 == 0) {
  #   OD_adjust <- FALSE
  #   Poisson_limits <- TRUE
  #
  #   )
  #
  # } else {
  #   stop("Invalid method supplied")
  # }

  dfCI<-build_limits_lookup(min_x=min_x, max_x=max_x, min_y=min_y, max_y=max_y, 
                            Poisson_limits=Poisson_limits, OD_adjust=OD_adjust, Tau2=Tau2, 
                            data_type=data_type, sr_method=sr_method, Target=Target, multiplier=multiplier)

  
  # base funnel plot
  funnel_p <- ggplot(mod_plot_agg, aes(y = multiplier * ((numerator / denominator)), x = denominator)) +
    geom_point(size = 2, alpha = 0.55, shape = 21, fill = "dodgerblue") +
    geom_hline(aes(yintercept = Target), linetype = 2) +
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
    if (Poisson_limits == TRUE & OD_adjust == FALSE) {
      funnel_p <- funnel_p +
        geom_line(aes(x = number.seq, y = ll95, col = "95% Poisson"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = ul95, col = "95% Poisson"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = ll998, col = "99.8% Poisson"), size = 1, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = ul998, col = "99.8% Poisson"), size = 1, data = dfCI, na.rm = TRUE) +
        scale_color_manual(values = c(
          "99.8% Poisson" = "#1F77B4FF", # "#1F77B4FF"
          "95% Poisson" = "#FF7F0EFF"
        ), name = "Control limits")
    }

    if (Poisson_limits == FALSE &  OD_adjust == TRUE) {
      funnel_p <- funnel_p +
        geom_line(aes(x = number.seq, y = odll95, col = "95% Overdispersed"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = odul95, col = "95% Overdispersed"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = odll998, col = "99.8% Overdispersed"), size = 1, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = number.seq, y = odul998, col = "99.8% Overdispersed"), size = 1, data = dfCI, na.rm = TRUE) +
        scale_color_manual(values = c(
          "99.8% Overdispersed" = "#2CA02CFF",
          "95% Overdispersed" = "#9467BDFF"
        ), name = "Control limits")
    }
  }

  # Apply plot scaling
  if (OD_adjust == TRUE) {
    funnel_p <- funnel_p +
      scale_y_continuous(name = y_label, limits = c(min_y, max_y))+
                         # limits = c(((max(min_y
                         #                  , (min(subset(mod_plot_agg
                         #                                , mod_plot_agg$denominator>4)$OD99LCL)*multiplier) - (multiplier*0.1)
                         #                  )
                         #              )
                         #             )
                         #            , ((max(max_y 
                         #                    , (max(subset(mod_plot_agg
                         #                                  , mod_plot_agg$denominator>4)$OD99UCL)*multiplier) - (multiplier*0.1))
                         #                )
                         #               )
                         #            )
                         # ) +
      scale_x_continuous(name = x_label, labels = scales::comma, limits = c(min_x -1, max_x + 1))
  } else {
    funnel_p <- funnel_p +
      scale_y_continuous(name = y_label, limits=c(min_y, max_y))+
                         # , limits = c(((max(min_y 
                         #                    , (min(subset(mod_plot_agg
                         #                                  , mod_plot_agg$denominator>4)$LCL99)*multiplier) - (multiplier*0.1))
                         #                )
                         #               )
                         #                )
                         #              , ((max(max_y
                         #                      , (max(subset(mod_plot_agg, mod_plot_agg$denminator >4)$UCL99)*multiplier) + (multiplier*0.1))
                         #                  )
                         #                 )
                         #              )
                         # ) +
      scale_x_continuous(name = x_label, labels = scales::comma, limits = c(min_x -1, max_x + 1))
  }

 # Label outliers
  if (label_outliers == 95) {
    if (OD_adjust==FALSE) {
      funnel_p <- funnel_p +
        ggrepel::geom_label_repel(aes(label = ifelse(rr > UCL95,
                                                     as.character(group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0) +
        ggrepel::geom_label_repel(aes(label = ifelse(rr < LCL95,
                                                     as.character(group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0)
    } else {
      funnel_p <- funnel_p +
        ggrepel::geom_label_repel(aes(label = ifelse(rr > OD95UCL,
                                                     as.character(group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0) +
        ggrepel::geom_label_repel(aes(label = ifelse(rr < OD95LCL,
                                                     as.character(group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0)
    }
  }
  if (label_outliers == 99) {
    if (OD_adjust==FALSE) {
      funnel_p <- funnel_p +
        ggrepel::geom_label_repel(aes(label = ifelse(rr > UCL99,
                                                     as.character(group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0) +
        ggrepel::geom_label_repel(aes(label = ifelse(rr < LCL99,
                                                     as.character(group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0)
    } else {
      funnel_p <- funnel_p +
        ggrepel::geom_label_repel(aes(label = ifelse(rr > OD99UCL,
                                                     as.character(group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0) +
        ggrepel::geom_label_repel(aes(label = ifelse(rr < OD99LCL,
                                                     as.character(group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0)
    }
  }

 return(list(funnel_p, dfCI))

}
