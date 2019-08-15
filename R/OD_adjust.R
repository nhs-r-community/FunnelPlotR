#' @title Winsorisation and adjusted z-score function
#' @description This is an implementation of funnel plots described Spiegelhalter (2005).
#' There are several parameters for the input, with the assumption that you will want smooth,
#'  overdispersed, funnel limits plotted based on the DerSimmonian Laird \eqn{\tau^2} additive random
#' effects models.
#'
#' @param input_frame A data.frame input of the plot data including numerator, denominator and rate/ratio
#' @param observed  A vector of the observed value.  Used as numerator of the Y-axis
#' @param group A vector of group names or a factor.  Used to aggreagate and group points on plots


## Winsorisation

winz_z<-function(input_frame=mod_plot_agg, method = "SHMI", Winsorize_by = 0.1){

if (method == "CQC") {
  mod_plot_agg <- mod_plot_agg %>%
    mutate(
      y = sqrt(observed / predicted),
      S = 1 / (2 * sqrt(predicted)),
      rrS2 = S^2,
      Uzscore_CQC = 2 * (sqrt(observed) - sqrt(predicted)),
      LCL95 = multiplier * (1 - (-1.959964 * sqrt(1 / rrS2))^2),
      UCL95 = multiplier * (1 + (1.959964 * sqrt(1 / rrS2))^2),
      LCL99 = multiplier * (1 - (-3.090232 * sqrt(1 / rrS2))^2),
      UCL99 = multiplier * (1 + (3.090232 * sqrt(1 / rrS2))^2)
    )
  
  
  lz <- quantile(x = mod_plot_agg$Uzscore_CQC, Winsorize_by)
  uz <- quantile(x = mod_plot_agg$Uzscore_CQC, (1 - Winsorize_by))
  
  mod_plot_agg <- mod_plot_agg %>%
    dplyr::mutate(
      Winsorised = ifelse(Uzscore_CQC > lz & Uzscore_CQC < uz, 0, 1),
      Wuzscore = ifelse(Uzscore_CQC < lz, lz, ifelse(Uzscore_CQC > uz, uz, Uzscore_CQC)),
      Wuzscore2 = Wuzscore^2
    )
  
  phi <- mod_plot_agg %>%
    dplyr::summarise(phi = (1 / as.numeric(n())) * sum(Wuzscore2)) %>%
    as.numeric()
  
  if(is.na(phi)){
    phi<-0
  }
  
  Tau2 <- mod_plot_agg %>%
    dplyr::summarise(Tau2 = max(
      0,
      ((n() * phi) - (n() - 1)) /
        
        (sum(1 / rrS2) - (sum(1 / (S)) / sum(1 / rrS2)))
    )) %>%
    as.numeric()
  
  if(is.na(Tau2)){
    Tau2<-0
  }
  
  mod_plot_agg <- mod_plot_agg %>%
    dplyr::mutate(
      phi = phi,
      Tau2 = Tau2,
      Wazscore = (y - 1) / sqrt(((1 / (2 * sqrt(predicted)))^2) + Tau2),
      OD95LCI = multiplier * ((1 - (-1.959964 * sqrt(((1 / (2 * sqrt(predicted)))^2) + Tau2)))^2),
      OD95UCI = multiplier * ((1 + (1.959964 * sqrt(((1 / (2 * sqrt(predicted)))^2) + Tau2)))^2),
      OD99LCI = multiplier * ((1 - (-3.090232 * sqrt(((1 / (2 * sqrt(predicted)))^2) + Tau2)))^2),
      OD99UCI = multiplier * ((1 + (3.090232 * sqrt(((1 / (2 * sqrt(predicted)))^2) + Tau2)))^2)
    )
} else if (method == "SHMI") {
  
  
  mod_plot_agg <- mod_plot_agg %>%
    mutate(
      s = 1 / (sqrt(predicted)),
      rrS2 = s^2,
      Uzscore_SHMI = sqrt(predicted) * log(observed / predicted),
      LCL95 = multiplier * (exp(-1.959964 * sqrt(rrS2))),
      UCL95 = multiplier * (exp(1.959964 * sqrt(rrS2))),
      LCL99 = multiplier * (exp(-3.090232 * sqrt(rrS2))),
      UCL99 = multiplier * (exp(3.090232 * sqrt(rrS2)))
    )
  
  lz <- quantile(x = mod_plot_agg$Uzscore_SHMI, Winsorize_by)
  uz <- quantile(x = mod_plot_agg$Uzscore_SHMI, (1 - Winsorize_by))
  
  mod_plot_agg <- mod_plot_agg %>%
    dplyr::mutate(Winsorised = ifelse(Uzscore_SHMI > lz & Uzscore_SHMI < uz, 0, 1))
  
  mod_plot_agg_sub <- mod_plot_agg %>%
    dplyr::filter(Winsorised == 0) %>%
    mutate(
      Wuzscore = Uzscore_SHMI,
      Wuzscore2 = Uzscore_SHMI^2
    )
  
  phi <- mod_plot_agg_sub %>%
    dplyr::summarise(phi = (1 / as.numeric(n())) * sum(Wuzscore2)) %>%
    as.numeric()
  
  if(is.na(phi)){
    phi<-0
  }
  
  Tau2 <- mod_plot_agg_sub %>%
    dplyr::summarise(Tau2 = max(0, ((n() * phi) - (n() - 1)) /
                                  (sum(predicted) - (sum(predicted^2) / sum(predicted))))) %>%
    as.numeric()
  
  if(is.na(Tau2)){
    Tau2<-0
  }
  
  mod_plot_agg <- mod_plot_agg %>%
    dplyr::mutate(
      Wuzscore = ifelse(Winsorised == 1, NA, Uzscore_SHMI),
      Wuzscore2 = ifelse(Winsorised == 1, NA, Uzscore_SHMI^2),
      phi = phi,
      Tau2 = Tau2,
      Wazscore = log(rr) / sqrt((1 / predicted) + Tau2),
      var = sqrt(rrS2 + Tau2),
      OD95LCI = multiplier * (exp(-1.959964 * sqrt((1 / predicted) + Tau2))),
      OD95UCI = multiplier * (exp(1.959964 * sqrt((1 / predicted) + Tau2))),
      OD99LCI = multiplier * (exp(-3.090232 * sqrt((1 / predicted) + Tau2))),
      OD99UCI = multiplier * (exp(3.090232 * sqrt((1 / predicted) + Tau2)))
    )
} else {
  stop("Please specify a valid method")
}
}