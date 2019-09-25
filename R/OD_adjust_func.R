#' Winsorisation and adjusted z-score function
#'
#' @description Internal function to perform the winsorisation and adjustment of z-scores prior to plotting.
#'
#' @param mod_plot_agg Aggregated model input data
#' @param method Adjustment method, can take the value \"SHMI\" or \"CQC\". \"SHMI\" is default.
#' @param Winsorise_by The amount to winsorise\/truncate the distribution by, prior to transformation. 0.1 means 10\% (at each end).
#' @param multiplier The amount to multiply the standardised ratio by, default is 1.
#' @param bypass TRUE\/FALSE, whether to bypass adjustment (not yet in use in main function)
#'
#' @return A list with elements\: aggregated adjusted data fame, Phi (a numeric dispersion ratio), Tau2 (a numeric \"between\" standard error)'
#' @keywords internal
#' @importFrom dplyr mutate summarise %>%
#' @importFrom dplyr group_by summarise mutate %>%
#' @importFrom rlang .data
#'
OD_adjust_func<-function(mod_plot_agg=mod_plot_agg, method = "SHMI", Winsorise_by = 0.1, multiplier=1, bypass=FALSE){

if(bypass==TRUE){
  return(list(mod_plot_agg, phi=as.numeric(0), Tau2=as.numeric(0)))
}

if(method == "CQC"){
  mod_plot_agg <- mod_plot_agg %>%
    mutate(
      y = sqrt(.data$numerator / .data$denominator),
      S = 1 / (2 * sqrt(.data$denominator)),
      rrS2 = .data$S^2,
      Uzscore_CQC = 2 * (sqrt(.data$numerator) - sqrt(.data$denominator)),
      #LCL95 = multiplier * ((1 - (-1.959964 * sqrt(((1 / (2 * sqrt(.data$denominator)))^2))))^2),
      LCL95 = multiplier * 1 - (1.959964 * .data$S),  #                  (exp(-1.959964 * sqrt((1 / .data$denominator) + Tau2)))
      UCL95 = multiplier * 1 + (1.959964 * .data$S),
      LCL99 = multiplier * 1 - (3.090232 * .data$S),
      UCL99 = multiplier * 1 + (3.090232 * .data$S),
      #LCL95 = multiplier * ((-1.959964 * sqrt(1 / .data$rrS2))^2),
      #UCL95 = multiplier * (1 + (1.959964 * sqrt(1 / .data$rrS2))^2),
      #ULCL95 =  multiplier * ((1 + (1.959964 * sqrt(((1 / (2 * sqrt(.data$denominator)))^2))))^2),
      #LCL99 = multiplier * (1 - (-3.090232 * sqrt(1 / .data$rrS2))^2),
      #UCL99 = multiplier * (1 + (3.090232 * sqrt(1 / .data$rrS2))^2),
    #  LCL99 = multiplier * ((1 - (-3.090232 * sqrt(((1 / (2 * sqrt(.data$denominator)))^2))))^2),
     # UCL99 = multiplier * ((1 + (3.090232 * sqrt(((1 / (2 * sqrt(.data$denominator)))^2))))^2)
    )


  lz <- quantile(x = mod_plot_agg$Uzscore_CQC, Winsorise_by, na.rm = TRUE)
  uz <- quantile(x = mod_plot_agg$Uzscore_CQC, (1 - Winsorise_by), na.rm = TRUE)

  mod_plot_agg <- mod_plot_agg %>%
    dplyr::mutate(
      Winsorised = ifelse(.data$Uzscore_CQC > lz & .data$Uzscore_CQC < uz, 0, 1),
      Wuzscore = ifelse(.data$Uzscore_CQC < lz, lz, ifelse(.data$Uzscore_CQC > uz, uz, .data$Uzscore_CQC)),
      Wuzscore2 = .data$Wuzscore^2
    )

  phi <- mod_plot_agg %>%
    dplyr::summarise(phi = (1 / as.numeric(n())) * sum(.data$Wuzscore2)) %>%
    as.numeric()

  if(is.na(phi) || bypass==TRUE){
    phi<-0
  }

  Tau2 <- mod_plot_agg %>%
    dplyr::summarise(Tau2 = max(
      0,
      ((n() * phi) - (n() - 1)) /

        (sum(1 / .data$rrS2) - (sum(1 / (.data$S)) / sum(1 / .data$rrS2)))
    )) %>%
    as.numeric()

  if(is.na(Tau2) || bypass==TRUE){
    Tau2<-0
  }

  mod_plot_agg <- mod_plot_agg %>%
    dplyr::mutate(
      phi = phi,
      Tau2 = Tau2,
      Wazscore = (.data$y - 1) / sqrt(((1 / (2 * sqrt(.data$denominator)))^2) + Tau2),
      # OD95LCL = multiplier * 1 - (1.959964 * sqrt(((.data$S^2) + Tau2))),
      # OD95UCL = multiplier * 1 + (1.959964 * sqrt(((.data$S^2) + Tau2))),
      # OD99LCL = multiplier * 1 - (3.090232 * sqrt(((.data$S^2) + Tau2))),
      # OD99UCL = multiplier * 1 + (3.090232 * sqrt(((.data$S^2) + Tau2)))
      # OD95LCL = multiplier * ((1 - (-1.959964 * sqrt((.data$S^2) + Tau2)))^2),
      # OD95UCL = multiplier * ((1 + (1.959964 * sqrt(((1 / (2 * sqrt(.data$denominator)))^2) + Tau2)))^2),
      # OD99LCL = multiplier * ((1 - (-3.090232 * sqrt(((1 / (2 * sqrt(.data$denominator)))^2) + Tau2)))^2),
      # OD99UCL = multiplier * ((1 + (3.090232 * sqrt(((1 / (2 * sqrt(.data$denominator)))^2) + Tau2)))^2)
      # OD95LCL = multiplier * ((1 + (-1.959964 * (sqrt((.data$S^2) + Tau2))))^2),
      # OD95UCL = multiplier * ((1 + sqrt(1.959964 * (sqrt((.data$S^2) + Tau2))))^2),
      # OD99LCL = multiplier * ((1 + sqrt(-3.090232 * (sqrt((.data$S^2) + Tau2))))^2),
      # OD99UCL = multiplier * ((1 + sqrt(3.090232 * (sqrt((.data$S^2) + Tau2))))^2),
      # OD95LCL = multiplier * (1 - ((1.959964 * (sqrt(((1 / (2 * sqrt(.data$S)))^2) + Tau2))))^2),
      # OD95UCL = multiplier * (1 + ((1.959964 * (sqrt(((1 / (2 * sqrt(.data$S)))^2) + Tau2))))^2),
      # OD99LCL = multiplier * (1 - ((3.090232 * (sqrt(((1 / (2 * sqrt(.data$S)))^2) + Tau2))))^2),
      # OD99UCL = multiplier * (1 + ((3.090232 * (sqrt(((1 / (2 * sqrt(.data$S)))^2) + Tau2))))^2)

      OD95LCL = multiplier * ((1 + (-1.959964 * (sqrt(((1 / (2 * sqrt(.data$denominator)))^2) + Tau2))))^2),
      OD95UCL = multiplier * ((1 + (1.959964 * (sqrt(((1 / (2 * sqrt(.data$denominator)))^2) + Tau2))))^2),
      OD99LCL = multiplier * ((1 + (-3.090232 * (sqrt(((1 / (2 * sqrt(.data$denominator)))^2) + Tau2))))^2),
      OD99UCL = multiplier * ((1 + (3.090232 * (sqrt(((1 / (2 * sqrt(.data$denominator)))^2) + Tau2))))^2)

    )
} else if (method == "SHMI") {

  mod_plot_agg <- mod_plot_agg %>%
    mutate(
      s = 1 / (sqrt(.data$denominator)),
      rrS2 = .data$s^2,
      Uzscore_SHMI = sqrt(.data$denominator) * log(.data$numerator / .data$denominator),
      LCL95 = multiplier * (exp(-1.959964 * sqrt(.data$rrS2))),
      UCL95 = multiplier * (exp(1.959964 * sqrt(.data$rrS2))),
      LCL99 = multiplier * (exp(-3.090232 * sqrt(.data$rrS2))),
      UCL99 = multiplier * (exp(3.090232 * sqrt(.data$rrS2)))
    )

  lz <- quantile(x = mod_plot_agg$Uzscore_SHMI, Winsorise_by, na.rm = TRUE)
  uz <- quantile(x = mod_plot_agg$Uzscore_SHMI, (1 - Winsorise_by), na.rm=TRUE)

  mod_plot_agg <- mod_plot_agg %>%
    dplyr::mutate(Winsorised = ifelse(.data$Uzscore_SHMI > lz & .data$Uzscore_SHMI < uz, 0, 1))

  mod_plot_agg_sub <- mod_plot_agg %>%
    dplyr::filter(.data$Winsorised == 0) %>%
    mutate(
      Wuzscore = .data$Uzscore_SHMI,
      Wuzscore2 = .data$Uzscore_SHMI^2
    )

  phi <- mod_plot_agg_sub %>%
    dplyr::summarise(phi = (1 / as.numeric(n())) * sum(.data$Wuzscore2)) %>%
    as.numeric()

  if(is.na(phi) || bypass==TRUE){
    phi<-0
  }

  Tau2 <- mod_plot_agg_sub %>%
    dplyr::summarise(Tau2 = max(0, ((n() * phi) - (n() - 1)) /
                                  (sum(.data$denominator) - (sum(.data$denominator^2) / sum(.data$denominator))))) %>%
    as.numeric()

  if(is.na(Tau2) || bypass==TRUE){
    Tau2<-0
  }

  mod_plot_agg <- mod_plot_agg %>%
    dplyr::mutate(
      Wuzscore = ifelse(.data$Winsorised == 1, NA, .data$Uzscore_SHMI),
      Wuzscore2 = ifelse(.data$Winsorised == 1, NA, .data$Uzscore_SHMI^2),
      phi = phi,
      Tau2 = Tau2,
      Wazscore = log(.data$rr) / sqrt((1 / .data$denominator) + Tau2),
      var = sqrt(.data$rrS2 + Tau2),
      OD95LCL = multiplier * (exp(-1.959964 * sqrt((1 / .data$denominator) + Tau2))),
      OD95UCL = multiplier * (exp(1.959964 * sqrt((1 / .data$denominator) + Tau2))),
      OD99LCL = multiplier * (exp(-3.090232 * sqrt((1 / .data$denominator) + Tau2))),
      OD99UCL = multiplier * (exp(3.090232 * sqrt((1 / .data$denominator) + Tau2)))
    )
  } else {
  stop("Please specify a valid method")
  }
  return(list(mod_plot_agg, phi, Tau2))
}
