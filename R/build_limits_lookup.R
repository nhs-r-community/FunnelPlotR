#' Function to build funnel limits
#' @description Internal function for funnel plot to build the control limits prior to plotting.
#' @param min_preds Minimum predicted value for range of x-axis
#' @param max_preds Maximum predicted value for range of x-axis
#' @param min_ratio Minimum ratio value for range of y-axis
#' @param max_ratio Maximum ratio value for range of y-axis
#' @param Poisson_limits TRUE/FALSE Draw Poisson distribution limits?
#' @param OD_adjust TRUE/FALSE Use overdispersion adjustment
#' @param Tau2 If using OD_adjust, what is the Tau2 ("between" standard error) to use?
#' @param data_type SR, PR or RC. Used to set target reference
#' @param sr_method Which adjustment method is being used, SHMI or CQC?
#' @param Target target to be used to set centre line
#' @param multiplier Multiply ratio value by an amount.  Default is 1, but some mortality ratios use 100, for example.
#' @keywords internal
#' @return A data.frame with an index column and various control limits based on the index as an x-axis value.
#'
#' @importFrom stats qchisq quantile
build_limits_lookup<-function(min_x, max_x, min_y, max_y, Poisson_limits
                              , OD_adjust, Tau2, data_type, sr_method
                              , Target, multiplier){
  
  
  # general limits + Tau2 limits table
  set.seed(1)
  
  if(data_type=="SR"){
  number.seq <- c(seq(1.1, as.numeric(max_x), length.out = 1000))
  } else {
    number.seq <- c(seq(3, as.numeric(max_x), length.out = 1000))
  }
  
  
  dfCI <- data.frame(
    number.seq,
    ll95 = multiplier * Target * ((qchisq(0.975, 2 * number.seq, lower.tail = FALSE) / 2) / number.seq),
    ul95 = multiplier * Target *((qchisq(0.025, 2 * (number.seq + 1), lower.tail = FALSE) / 2) / number.seq),
    ll998 = multiplier * Target *((qchisq(0.999, 2 * number.seq, lower.tail = FALSE) / 2) / number.seq),
    ul998 = multiplier * Target *((qchisq(0.001, 2 * (number.seq + 1), lower.tail = FALSE) / 2) / number.seq)
  )

  ### Calculate funnel limits ####
  if (OD_adjust == FALSE) {
    Poisson_limits <- TRUE
   # message("OD_adjust set to FALSE, plotting using Poisson limits")
  }
  
  
  if (OD_adjust == TRUE & Tau2 == 0){
    OD_adjust <- FALSE
    Poisson_limits <- TRUE
  }
    
  if (OD_adjust == FALSE){
    message("No adjustment for overdispersion made")
    return(dfCI)
    
  }
  
  if (data_type=="SR" & sr_method == "SHMI"){
    dfCI$s <-sqrt(1/number.seq)
    
    dfCI$odll95 <- multiplier * (exp(-1.959964 * sqrt((1 / number.seq) + Tau2)))
    dfCI$odul95 <- multiplier * (exp(1.959964 * sqrt((1 / number.seq) + Tau2)))
    dfCI$odll998 <- multiplier * (exp(-3.090232 * sqrt((1 / number.seq) + Tau2)))
    dfCI$odul998 <- multiplier * (exp(3.090232 * sqrt((1 / number.seq) + Tau2)))
    
    
  }  else if (data_type=="SR" & sr_method == "CQC"){
    
    # PR 1/2*sqrt(n) and CQC SR methods 1/2*sqrt(E)
    #Target = 1 
    dfCI$s <- 1/(2*sqrt(number.seq))
    
    dfCI$odll95 <- multiplier * (1 - (1.959964 * sqrt(dfCI$s^2 + Tau2))^2)
    dfCI$odul95 <- multiplier * (1 + (1.959964 * sqrt(dfCI$s^2 + Tau2))^2)
    dfCI$odll998 <- multiplier * (1 - (3.090232 * sqrt(dfCI$s^2 + Tau2))^2)
    dfCI$odul998 <- multiplier * (1 + (3.090232 * sqrt(dfCI$s^2 + Tau2))^2)
    
    
  } else if(data_type=="RC"){
    
    dfCI$s <-sqrt((number.seq/((number.seq+0.5)^2))  
                  +
                   (number.seq/((number.seq+0.5)^2)))
    
    dfCI$odll95 <- multiplier * (exp( log(Target) -(1.959964 * sqrt(dfCI$s^2 + Tau2))))
    dfCI$odul95 <- multiplier * (exp( log(Target) + (1.959964 * sqrt(dfCI$s^2 + Tau2))))
    dfCI$odll998 <- multiplier * (exp( log(Target) - (3.090232 * sqrt(dfCI$s^2 + Tau2))))
    dfCI$odul998 <- multiplier * (exp( log(Target) + (3.090232 * sqrt(dfCI$s^2 + Tau2))))

    
  } else if (data_type=="PR"){
    
    # PR 1/2*sqrt(n) and CQC SR methods 1/2*sqrt(E)
    dfCI$s <- 1/(2*sqrt(number.seq))
    
    dfCI$odll95 <- multiplier * sin(asin(sqrt(Target)) + 1.959964 * sqrt((dfCI$s^2) +Tau2))^2
    dfCI$odul95 <- multiplier * sin(asin(sqrt(Target)) - 1.959964 * sqrt((dfCI$s^2) +Tau2))^2
    dfCI$odll998 <- multiplier * sin(asin(sqrt(Target)) + 3.090232 * sqrt((dfCI$s^2) +Tau2))^2
    dfCI$odul998 <- multiplier * sin(asin(sqrt(Target)) - 3.090232 * sqrt((dfCI$s^2) +Tau2))^2
    
  }

  return(dfCI)
}


