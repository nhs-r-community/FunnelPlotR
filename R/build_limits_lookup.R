#' Function to build funnel limits
#'
#' @param max_preds 
#' @param min_preds 
#' @param min_ratio 
#' @param max_ratio 
#' @param OD_Tau2 
#' @param Poisson_limits 
#'
#' @return
#' @export
#'
#' @examples
build_limits_lookup<-function(max_preds, min_preds, min_ratio, max_ratio, OD_Tau2, Poisson_limits){
  
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
