#' Overdispersion-adjusted funnel limit calculation
#'
#' @description Add 95% and 99.8 % funnel limits from OD-adjusted Poisson distribution
#'
#' @param dfCI Aggregated model input data
#' @param data_type Type of data for adjustment and plotting: Indirectly Standardised ratio (\"SR\"), proportion (\"PR\"), or ratio of counts (\"RC\").
#' @param adjust_method Adjustment method for standardised ratios, can take the value \"SHMI\" or \"CQC\". \"SHMI\" is default. 
#' @param multiplier Multiplier to adjust limits if reporting by a multiplier, e.g. per 1000.
#' @param tau2 A 'between' standard deviation to add to the within standard deviation, S, to inflate limits.
#' @param target The centre line of the plot. Mean for non-SRs or 1 for SR
#' @keywords internal
#' @return A data.frame of with appended OD limits
#' 
calculate_limits<-function(dfCI=dfCI, data_type = "SR", adjust_method = "SHMI", multiplier = 1, tau2 = 0
                           ,target=target, draw_adjusted=draw_adjusted){
  # Construct variable names for limits
  od = ifelse(draw_adjusted == TRUE, "od", "")
  var_names = paste0(od, c("ll95","ul95","ll998","ul998"))

  # Set between-institution variance to zero for calculating approximate
  #   unadjusted limits for PR and RC data
  tau2 = ifelse(draw_adjusted == FALSE, 0, tau2)

  if(data_type == "SR"){
    if(draw_adjusted == FALSE) {
      # Identify number of existing columns, as we will append the calculated limits the end
      ncols = ncol(dfCI)
      dfCI[,ncols + 1] <- multiplier * target * (qchisq(0.975, 2*dfCI$number.seq, lower.tail = FALSE)/2)/ dfCI$number.seq
      dfCI[,ncols + 2] <- multiplier * target * (qchisq(0.025, 2*(dfCI$number.seq+1), lower.tail = FALSE)/2) / dfCI$number.seq
      dfCI[,ncols + 3] <- multiplier * target * (qchisq(0.999, 2*dfCI$number.seq, lower.tail = FALSE)/2)/ dfCI$number.seq
      dfCI[,ncols + 4] <- multiplier * target * (qchisq(0.001, 2*(dfCI$number.seq+1), lower.tail = FALSE)/2) / dfCI$number.seq
      
    } else if(adjust_method == "SHMI"){
      dfCI$s <-sqrt(1/dfCI$number.seq)
      ncols = ncol(dfCI)

      dfCI[,ncols + 1] <- multiplier * (exp(-1.959964 * sqrt((1/dfCI$number.seq) + tau2)))
      dfCI[,ncols + 2] <- multiplier * (exp(1.959964  * sqrt((1/dfCI$number.seq) + tau2)))
      dfCI[,ncols + 3] <- multiplier * (exp(-3.090232 * sqrt((1/dfCI$number.seq) + tau2)))
      dfCI[,ncols + 4] <- multiplier * (exp(3.090232  * sqrt((1/dfCI$number.seq) + tau2)))

    } else {
      dfCI$s <- 1/(2*sqrt(dfCI$number.seq))
      ncols = ncol(dfCI)

      dfCI[,ncols + 1] <- multiplier * (1 - (1.959964 * sqrt( (dfCI$s^2 + tau2))))
      dfCI[,ncols + 2] <- multiplier * (1 + (1.959964 * sqrt( (dfCI$s^2 + tau2))))
      dfCI[,ncols + 3] <- multiplier * (1 - (3.090232 * sqrt( (dfCI$s^2 + tau2))))
      dfCI[,ncols + 4] <- multiplier * (1 + (3.090232 * sqrt( (dfCI$s^2 + tau2))))
    }
    
  } else if(data_type=="RC"){
    dfCI$s <- 1/(2*sqrt(dfCI$number.seq))
    ncols = ncol(dfCI)
    
    dfCI[,ncols + 1] <- multiplier * (exp( log(target) - (1.959964 * sqrt( dfCI$s^2 + tau2))))
    dfCI[,ncols + 2] <- multiplier * (exp( log(target) + (1.959964 * sqrt( dfCI$s^2 + tau2))))
    dfCI[,ncols + 3] <- multiplier * (exp( log(target) - (3.090232 * sqrt( dfCI$s^2 + tau2))))
    dfCI[,ncols + 4] <- multiplier * (exp( log(target) + (3.090232 * sqrt( dfCI$s^2 + tau2))))

  } else if(data_type=="PR"){
    dfCI$s <- 1/(2*sqrt(dfCI$number.seq))
    ncols = ncol(dfCI)
    
    dfCI[,ncols + 1] <-  multiplier * (sin(asin(sqrt(target)) - 1.959964 * sqrt((dfCI$s^2) +tau2))^2)
    dfCI[,ncols + 2] <-  multiplier * (sin(asin(sqrt(target)) + 1.959964 * sqrt((dfCI$s^2) +tau2))^2)
    dfCI[,ncols + 3] <-  multiplier * (sin(asin(sqrt(target)) - 3.090232 * sqrt((dfCI$s^2) +tau2))^2)
    dfCI[,ncols + 4] <-  multiplier * (sin(asin(sqrt(target)) + 3.090232 * sqrt((dfCI$s^2) +tau2))^2)

  }

  # Add variable names
  colnames(dfCI) = c(colnames(dfCI)[1:ncols],var_names)

  return(dfCI)
}

