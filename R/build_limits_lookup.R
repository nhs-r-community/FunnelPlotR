#' Function to build funnel limits
#' @description Internal function for funnel plot to build the control limits prior to plotting.
#' @param min_preds Minimum predicted value for range of x-axis
#' @param max_preds Maximum predicted value for range of x-axis
#' @param min_ratio Minimum ratio value for range of y-axis
#' @param max_ratio Maximum ratio value for range of y-axis
#' @param Poisson_limits TRUE/FALSE Draw Poisson distribution limits?
#' @param OD_adjust TRUE/FALSE USe over dispersion adjustment
#' @param Tau2 If using OD_adjustment, what is the Tau2 ("between" standard error) to use?
#' @param method Which adjustment method is being used, SHMI or CQC?
#' @param multiplier Multiply ratio vaue by and amount.  Default is 1, but some mortality ratios use 100, for example.
#'
#' @return A data.frame with an index column and various control limits based on the index as an x-axis value.
#'
#' @importFrom stats qchisq quantile
build_limits_lookup<-function(min_preds, max_preds, min_ratio, max_ratio, Poisson_limits, OD_adjust, Tau2, method, multiplier){

  ## quiets concerns of R CMD check re: the .'s that appear in pipelines
  if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

  ### Calculate funnel limits ####
  if (OD_adjust == FALSE) {
    Poisson_limits <- TRUE
   # message("OD_adjust set to FALSE, plotting using Poisson limits")
  }

  if (OD_adjust == TRUE & Tau2 == 0) {
    OD_adjust <- FALSE
    Poisson_limits <- TRUE

    #message("No overdispersion detected, or OD_adjust set to FALSE, plotting using Poisson limits")

    # general limits + Tau2 limits table
    set.seed(1)
    number.seq <- c(seq(0.1, 10, 0.1), seq(11.0, max_preds, 1))
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
    number.seq <- c(seq(0.1, 10, 0.1), seq(11.0, max_preds, 1))
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
    number.seq <- seq(1, ceiling(as.numeric(max_preds)), 1)
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

  return(dfCI)
}
