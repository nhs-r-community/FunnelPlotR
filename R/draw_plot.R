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

draw_plot<-function(){
  
### Calculate funnel limits ####
if (OD_Tau2 == FALSE) {
  Poisson_limits <- TRUE
  message("OD_Tau2 set to FALSE, plotting using Poisson limits")
}

if (OD_Tau2 == TRUE & Tau2 == 0) {
  OD_Tau2 <- FALSE
  Poisson_limits <- TRUE
  
  message("No overdispersion detected, or OD_Tau2 set to FALSE, plotting using Poisson limits")
  
  # general limits + Tau2 limits table
  set.seed(1)
  number.seq <- c(seq(0.1, 10, 0.1), seq(1, ceiling(max_preds), 1))
  dfCI <- data.frame(
    number.seq,
    ll95 = multiplier * ((qchisq(0.975, 2 * number.seq, lower.tail = FALSE) / 2) / number.seq),
    ul95 = multiplier * ((qchisq(0.025, 2 * (number.seq + 1), lower.tail = FALSE) / 2) / number.seq),
    ll998 = multiplier * ((qchisq(0.998, 2 * number.seq, lower.tail = FALSE) / 2) / number.seq),
    ul998 = multiplier * ((qchisq(0.001, 2 * (number.seq + 1), lower.tail = FALSE) / 2) / number.seq)
  )
} else if (method == "SHMI") {
  # general limits + Tau2 limits table
  set.seed(1)
  number.seq <- c(seq(0.1, 10, 0.1), seq(1, ceiling(max_preds), 1))
  dfCI <- data.frame(
    number.seq,
    ll95 = multiplier * ((qchisq(0.975, 2 * number.seq, lower.tail = FALSE) / 2) / number.seq),
    ul95 = multiplier * ((qchisq(0.025, 2 * (number.seq + 1), lower.tail = FALSE) / 2) / number.seq),
    ll998 = multiplier * ((qchisq(0.998, 2 * number.seq, lower.tail = FALSE) / 2) / number.seq),
    ul998 = multiplier * ((qchisq(0.001, 2 * (number.seq + 1), lower.tail = FALSE) / 2) / number.seq),
    odll95 = multiplier * (exp(-1.959964 * sqrt((1 / number.seq) + Tau2))),
    odul95 = multiplier * (exp(1.959964 * sqrt((1 / number.seq) + Tau2))),
    odll998 = multiplier * (exp(-3.090232 * sqrt((1 / number.seq) + Tau2))),
    odul998 = multiplier * (exp(3.090232 * sqrt((1 / number.seq) + Tau2)))
  )
} else if (method == "CQC") {
  set.seed(1)
  number.seq <- seq(1, ceiling(max_preds), 1)
  dfCI <- data.frame(
    number.seq,
    ll95 = multiplier * ((qchisq(0.975, 2 * number.seq, lower.tail = FALSE) / 2) / number.seq),
    ul95 = multiplier * ((qchisq(0.025, 2 * (number.seq + 1), lower.tail = FALSE) / 2) / number.seq),
    ll998 = multiplier * ((qchisq(0.998, 2 * number.seq, lower.tail = FALSE) / 2) / number.seq),
    ul998 = multiplier * ((qchisq(0.001, 2 * (number.seq + 1), lower.tail = FALSE) / 2) / number.seq),
    odll95 = multiplier * ((1 - (1.959964 * (sqrt(((1 / (2 * sqrt(number.seq)))^2) + Tau2))))^2),
    odul95 = multiplier * ((1 + (1.959964 * (sqrt(((1 / (2 * sqrt(number.seq)))^2) + Tau2))))^2),
    odll998 = multiplier * ((1 + (-3.090232 * (sqrt(((1 / (2 * sqrt(number.seq)))^2) + Tau2))))^2),
    odul998 = multiplier * ((1 + (3.090232 * (sqrt(((1 / (2 * sqrt(number.seq)))^2) + Tau2))))^2)
  )
} else {
  stop("Invalid method supplied")
}



# base funnel plot
funnel_p <- ggplot(mod_plot_agg, aes(y = multiplier * ((observed / predicted)), x = predicted)) +
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
    scale_y_continuous(name = y_label, limits = c((multiplier * (min(min_ratio - 0.05, min(subset(mod_plot_agg, observed>4)$OD99LCI) -0.1))), (multiplier * (max(max_ratio + 0.05, max(subset(mod_plot_agg, observed>4)$OD99UCI) - 0.1))))) +
    scale_x_continuous(name = x_label, labels = scales::comma, limits = c(min_preds -1, max_preds + 1))
} else {
  funnel_p <- funnel_p +
    scale_y_continuous(name = y_label, limits = c((multiplier * (min(min_ratio - 0.05, min(subset(mod_plot_agg, observed>4)$LCL99) - 0.1))), (multiplier * (max(max_ratio + 0.05, max(subset(mod_plot_agg, observed >4)$UCL99) + 0.1))))) +
    scale_x_continuous(name = x_label, labels = scales::comma, limits = c(min_preds -1, max_preds + 1))
}


if (label_outliers == 95) {
  if (OD_Tau2 == FALSE) {
    funnel_p <- funnel_p +
      ggrepel::geom_label_repel(aes(label = ifelse(observed / predicted > UCL95, as.character(grp), "")), size = 2.7, direction = "y") +
      ggrepel::geom_label_repel(aes(label = ifelse(observed / predicted < LCL95, as.character(grp), "")), size = 2.7, direction = "y")
  } else {
    funnel_p <- funnel_p +
      ggrepel::geom_label_repel(aes(label = ifelse(observed / predicted > OD95UCI, as.character(grp), "")), size = 2.7, direction = "y") +
      ggrepel::geom_label_repel(aes(label = ifelse(observed / predicted < OD95LCI, as.character(grp), "")), size = 2.7, direction = "y")
  }
}
if (label_outliers == 99) {
  if (OD_Tau2 == FALSE) {
    funnel_p <- funnel_p +
      ggrepel::geom_label_repel(aes(label = ifelse(observed / predicted > UCL99, as.character(grp), "")), size = 2.7, direction = "y") +
      ggrepel::geom_label_repel(aes(label = ifelse(observed / predicted < LCL99, as.character(grp), "")), size = 2.7, direction = "y")
  } else {
    funnel_p <- funnel_p +
      ggrepel::geom_label_repel(aes(label = ifelse(observed / predicted > OD99UCI, as.character(grp), "")), size = 2.7, direction = "y") +
      ggrepel::geom_label_repel(aes(label = ifelse(observed / predicted < OD99LCI, as.character(grp), "")), size = 2.7, direction = "y")
  }
}


}