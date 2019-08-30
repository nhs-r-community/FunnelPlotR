#' @title Funnel Plots drawing function
#' @description Internal function for drawing plot.  Do not use this directly, call `funnel_plot()` instead.
#'
#' @param mod_plot_agg data frame of containing numerator, denominator, ratio/proportion, SEs and limits
#' @param yrange Specify the plot range.  Not yet implemented
#' @param xrange Specify the plot range.  Not yet implemented
#' @param x_label Title for the funnel plot x-axis.  Usually expected deaths, readmissions, incidents etc.
#' @param y_label Title for the funnel plot y-axis.  Usually a standardised ratio.
#' @param title Plot title
#' @param label_outliers Add group labels to outliers on plot. Accepted values are\: 95 or 99 corresponding to 95\% or 99.8\% quantiles of the distribution. Default=99
#' @param multiplier Scale relative risk and funnel by this factor. Default to 1, but 100 is used for HSMR
#' @param Poisson_limits Draw exact limits based only on data points with no iterpolation. (default=FALSE)
#' @param OD_adjust Draw overdispersed limits using Speigelhalter's (2012) Tau2 (default=TRUE)
#' @param Tau2 The Tau2 value to use for plotting limits
#' @param method to pass to limit calculation (\"SHMI\" or \"CQC\")
#'
#' @return A list containing [1] the funnel plot as a ggplot2 object., [2]the limits table.
#'
#'
#' @importFrom scales comma
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang .data
#' @import ggplot2

draw_plot<-function(mod_plot_agg, yrange, xrange, x_label, y_label, title,
                    label_outliers, multiplier, Poisson_limits, OD_adjust, Tau2 = 0, method){

#plot ranges
  # Determine the range of plots
  max_preds <- dplyr::summarise(mod_plot_agg, ceiling(max(.data$denominator, na.rm = FALSE))) %>% as.numeric()
  min_preds <- dplyr::summarise(mod_plot_agg, ceiling(min(.data$denominator,na.rm = FALSE))) %>% as.numeric()
  min_ratio <- max((0.7 * multiplier), dplyr::summarise(mod_plot_agg, multiplier *
                                                          max((.data$numerator / .data$denominator))) %>% as.numeric(), na.rm = FALSE)

  max_ratio <- max((1.3 * multiplier), dplyr::summarise(mod_plot_agg, multiplier *
                                                        max((.data$numerator / .data$denominator))) %>% as.numeric(), na.rm = FALSE)


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

  dfCI<-build_limits_lookup(min_preds, max_preds, min_ratio, max_ratio, Poisson_limits, OD_adjust, Tau2, method, multiplier)


  # base funnel plot
  funnel_p <- ggplot(mod_plot_agg, aes(y = multiplier * ((.data$numerator / .data$denominator)), x = .data$denominator)) +
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




  if (Poisson_limits == TRUE & OD_adjust == TRUE) {
    funnel_p <- funnel_p +
      geom_line(aes(x = .data$number.seq, y = .data$ll95, col = "95% Poisson"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = .data$number.seq, y = .data$ul95, col = "95% Poisson"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = .data$number.seq, y = .data$ll998, col = "99.8% Poisson"), size = 1, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = .data$number.seq, y = .data$ul998, col = "99.8% Poisson"), size = 1, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = .data$number.seq, y = .data$odll95, col = "95% Overdispersed"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = .data$number.seq, y = .data$odul95, col = "95% Overdispersed"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = .data$number.seq, y = .data$odll998, col = "99.8% Overdispersed"), size = 1, data = dfCI, na.rm = TRUE) +
      geom_line(aes(x = .data$number.seq, y = .data$odul998, col = "99.8% Overdispersed"), size = 1, data = dfCI, na.rm = TRUE) +
      scale_color_manual(values = c(
        "99.8% Poisson" = "#1F77B4FF",
        "95% Poisson" = "#FF7F0EFF",
        "99.8% Overdispersed" = "#2CA02CFF",
        "95% Overdispersed" = "#9467BDFF"
      ), name = "Control limits")
  } else {
    if (Poisson_limits == TRUE & OD_adjust == FALSE) {
      funnel_p <- funnel_p +
        geom_line(aes(x = .data$number.seq, y = .data$ll95, col = "95% Poisson"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = .data$number.seq, y = .data$ul95, col = "95% Poisson"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = .data$number.seq, y = .data$ll998, col = "99.8% Poisson"), size = 1, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = .data$number.seq, y = .data$ul998, col = "99.8% Poisson"), size = 1, data = dfCI, na.rm = TRUE) +
        scale_color_manual(values = c(
          "99.8% Poisson" = "#7E9C06", # "#1F77B4FF"
          "95% Poisson" = "#FF7F0EFF"
        ), name = "Control limits")
    }

    if (Poisson_limits == FALSE &  OD_adjust == TRUE) {
      funnel_p <- funnel_p +
        geom_line(aes(x = .data$number.seq, y = .data$odll95, col = "95% Overdispersed"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = .data$number.seq, y = .data$odul95, col = "95% Overdispersed"), size = 1, linetype = 2, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = .data$number.seq, y = .data$odll998, col = "99.8% Overdispersed"), size = 1, data = dfCI, na.rm = TRUE) +
        geom_line(aes(x = .data$number.seq, y = .data$odul998, col = "99.8% Overdispersed"), size = 1, data = dfCI, na.rm = TRUE) +
        scale_color_manual(values = c(
          "99.8% Overdispersed" = "#DEC400", # "#F7EF0A", #"#2CA02CFF",
          "95% Overdispersed" = "#9467BDFF"
        ), name = "Control limits")
    }
  }


  if (OD_adjust == TRUE) {
    funnel_p <- funnel_p +
      scale_y_continuous(name = y_label, limits = c(((min(min_ratio - (multiplier*0.05), (min(subset(mod_plot_agg, mod_plot_agg$numerator>4)$OD99LCL)*multiplier) - (multiplier*0.1)))), ((max(max_ratio + (multiplier*0.05), (max(subset(mod_plot_agg, mod_plot_agg$numerator>4)$OD99UCL)*multiplier) - (multiplier*0.1)))))) +
      scale_x_continuous(name = x_label, labels = scales::comma, limits = c(min_preds -1, max_preds + 1))
  } else {
    funnel_p <- funnel_p +
      scale_y_continuous(name = y_label, limits = c(((min(min_ratio - (multiplier*0.05), (min(subset(mod_plot_agg, mod_plot_agg$numerator>4)$LCL99)*multiplier) - (multiplier*0.1)))), ((max(max_ratio + (multiplier*0.05), (max(subset(mod_plot_agg, mod_plot_agg$numerator >4)$UCL99)*multiplier) + (multiplier*0.1)))))) +
      scale_x_continuous(name = x_label, labels = scales::comma, limits = c(min_preds -1, max_preds + 1))
  }


  if (label_outliers == 95) {
    if (Tau2 ==0) {
      funnel_p <- funnel_p +
        ggrepel::geom_label_repel(aes(label = ifelse(.data$numerator / .data$denominator > .data$UCL95, 
                                                     as.character(.data$group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0) +
        ggrepel::geom_label_repel(aes(label = ifelse(.data$numerator / .data$denominator < .data$LCL95, 
                                                     as.character(.data$group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0)
    } else {
      funnel_p <- funnel_p +
        ggrepel::geom_label_repel(aes(label = ifelse(.data$numerator / .data$denominator > .data$OD95UCL, 
                                                     as.character(.data$group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0) +
        ggrepel::geom_label_repel(aes(label = ifelse(.data$numerator / .data$denominator < .data$OD95LCL, 
                                                     as.character(.data$group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0)
    }
  }
  if (label_outliers == 99) {
    if (Tau2 == 0) {
      funnel_p <- funnel_p +
        ggrepel::geom_label_repel(aes(label = ifelse(.data$numerator / .data$denominator > .data$UCL99, 
                                                     as.character(.data$group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0) +
        ggrepel::geom_label_repel(aes(label = ifelse(.data$numerator / .data$denominator < .data$LCL99, 
                                                     as.character(.data$group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0)
    } else {
      funnel_p <- funnel_p +
        ggrepel::geom_label_repel(aes(label = ifelse(.data$numerator / .data$denominator > .data$OD99UCL, 
                                                     as.character(.data$group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0) +
        ggrepel::geom_label_repel(aes(label = ifelse(.data$numerator / .data$denominator < .data$OD99LCL, 
                                                     as.character(.data$group), "")), size = 2.7, direction = "y",
                                  force = 2, min.segment.length = 0)
    }
  }

 return(list(funnel_p, dfCI))

}
