#' Winsorisation and adjusted z-score function
#'
#' @description Internal function to perform the winsorisation and adjustment of z-scores prior to funnel plots.
#'
#' @param mod_plot_agg Aggregated model input data
#' @param method Adjustment method, can take the value \"SHMI\" or \"CQC\". \"SHMI\" is default.
#' @param Winsorise_by The amount to winsorise\/truncate the distribution by, prior to transformation. 0.1 means 10\% (at each end).
#' @param multiplier The amount to multiply the standardised ratio by, default is 1.
#' @param bypass TRUE\/FALSE, whether to bypass adjustment (not yet in use in main function)
#'
#' @return A list with elements\: aggregated adjusted data fame, Phi (a numeric dispersion ratio), Tau2 (a numeric \"between\" standard error)' @importFrom dplyr mutate summarise %>%
#' @importFrom dplyr group_by summarise mutate %>%
#'
OD_adjust_func<-function(mod_plot_agg=mod_plot_agg, method = "SHMI", Winsorise_by = 0.1, multiplier=1, bypass=FALSE){

if(bypass==TRUE){
  return(list(mod_plot_agg, phi=as.numeric(0), Tau2=as.numeric(0)))
}

if(method == "CQC"){
  mod_plot_agg <- mod_plot_agg %>%
    mutate(
      y = sqrt(numerator / denominator),
      S = 1 / (2 * sqrt(denominator)),
      rrS2 = S^2,
      Uzscore_CQC = 2 * (sqrt(numerator) - sqrt(denominator)),
      LCL95 = multiplier * (1 - (-1.959964 * sqrt(1 / rrS2))^2),
      UCL95 = multiplier * (1 + (1.959964 * sqrt(1 / rrS2))^2),
      LCL99 = multiplier * (1 - (-3.090232 * sqrt(1 / rrS2))^2),
      UCL99 = multiplier * (1 + (3.090232 * sqrt(1 / rrS2))^2)
    )


  lz <- quantile(x = mod_plot_agg$Uzscore_CQC, Winsorise_by)
  uz <- quantile(x = mod_plot_agg$Uzscore_CQC, (1 - Winsorise_by))

  mod_plot_agg <- mod_plot_agg %>%
    dplyr::mutate(
      Winsorised = ifelse(Uzscore_CQC > lz & Uzscore_CQC < uz, 0, 1),
      Wuzscore = ifelse(Uzscore_CQC < lz, lz, ifelse(Uzscore_CQC > uz, uz, Uzscore_CQC)),
      Wuzscore2 = Wuzscore^2
    )

  phi <- mod_plot_agg %>%
    dplyr::summarise(phi = (1 / as.numeric(n())) * sum(Wuzscore2)) %>%
    as.numeric()

  if(is.na(phi) || bypass==TRUE){
    phi<-0
  }

  Tau2 <- mod_plot_agg %>%
    dplyr::summarise(Tau2 = max(
      0,
      ((n() * phi) - (n() - 1)) /

        (sum(1 / rrS2) - (sum(1 / (S)) / sum(1 / rrS2)))
    )) %>%
    as.numeric()

  if(is.na(Tau2) || bypass==TRUE){
    Tau2<-0
  }

  mod_plot_agg <- mod_plot_agg %>%
    dplyr::mutate(
      phi = phi,
      Tau2 = Tau2,
      Wazscore = (y - 1) / sqrt(((1 / (2 * sqrt(denominator)))^2) + Tau2),
      OD95LCL = multiplier * ((1 - (-1.959964 * sqrt(((1 / (2 * sqrt(denominator)))^2) + Tau2)))^2),
      OD95UCL = multiplier * ((1 + (1.959964 * sqrt(((1 / (2 * sqrt(denominator)))^2) + Tau2)))^2),
      OD99LCL = multiplier * ((1 - (-3.090232 * sqrt(((1 / (2 * sqrt(denominator)))^2) + Tau2)))^2),
      OD99UCL = multiplier * ((1 + (3.090232 * sqrt(((1 / (2 * sqrt(denominator)))^2) + Tau2)))^2)
    )
} else if (method == "SHMI") {

  mod_plot_agg <- mod_plot_agg %>%
    mutate(
      s = 1 / (sqrt(denominator)),
      rrS2 = s^2,
      Uzscore_SHMI = sqrt(denominator) * log(numerator / denominator),
      LCL95 = multiplier * (exp(-1.959964 * sqrt(rrS2))),
      UCL95 = multiplier * (exp(1.959964 * sqrt(rrS2))),
      LCL99 = multiplier * (exp(-3.090232 * sqrt(rrS2))),
      UCL99 = multiplier * (exp(3.090232 * sqrt(rrS2)))
    )

  lz <- quantile(x = mod_plot_agg$Uzscore_SHMI, Winsorise_by)
  uz <- quantile(x = mod_plot_agg$Uzscore_SHMI, (1 - Winsorise_by))

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

  if(is.na(phi) || bypass==TRUE){
    phi<-0
  }

  Tau2 <- mod_plot_agg_sub %>%
    dplyr::summarise(Tau2 = max(0, ((n() * phi) - (n() - 1)) /
                                  (sum(denominator) - (sum(denominator^2) / sum(denominator))))) %>%
    as.numeric()

  if(is.na(Tau2) || bypass==TRUE){
    Tau2<-0
  }

  mod_plot_agg <- mod_plot_agg %>%
    dplyr::mutate(
      Wuzscore = ifelse(Winsorised == 1, NA, Uzscore_SHMI),
      Wuzscore2 = ifelse(Winsorised == 1, NA, Uzscore_SHMI^2),
      phi = phi,
      Tau2 = Tau2,
      Wazscore = log(rr) / sqrt((1 / denominator) + Tau2),
      var = sqrt(rrS2 + Tau2),
      OD95LCL = multiplier * (exp(-1.959964 * sqrt((1 / denominator) + Tau2))),
      OD95UCL = multiplier * (exp(1.959964 * sqrt((1 / denominator) + Tau2))),
      OD99LCL = multiplier * (exp(-3.090232 * sqrt((1 / denominator) + Tau2))),
      OD99UCL = multiplier * (exp(3.090232 * sqrt((1 / denominator) + Tau2)))
    )
  } else {
  stop("Please specify a valid method")
  }
  return(list(mod_plot_agg, phi, Tau2))
}
