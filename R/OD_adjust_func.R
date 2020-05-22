#' Transformation function for z-scoring
#'
#' @description Internal function to perform the transformations for data types.
#'
#' @param mod_plot_agg Aggregated model input data
#' @param data_type Type of data for adjustment and plotting: Indirectly Standardised ratio (\"SR\"), proportion (\"PR\"), or ratio of counts (\"RC\").
#' @param sr_method Adjustment method, can take the value \"SHMI\" or \"CQC\". \"SHMI\" is default. 
#'
#' @return A data.frame of original, aggreagated data plus transformed z-score (unadjusted for overdispersion)
#' @keywords internal
#' 
transformed_zscore<-function(mod_plot_agg=mod_plot_agg, data_type = "SR", sr_method = "SHMI"){

  if(data_type == "SR"){
    
    # log-transformed SHMI verison
    if(sr_method == "SHMI"){
      mod_plot_agg$Target<- 0
      mod_plot_agg$Y <- log(mod_plot_agg$numerator / mod_plot_agg$denominator)
      mod_plot_agg$s <- 1 / (sqrt(mod_plot_agg$denominator))
      
    # sQRT-transformed CQC version
    } 
    
    if(sr_method == "CQC"){
      mod_plot_agg$Target<- 1
      mod_plot_agg$Y <- sqrt(mod_plot_agg$numerator / mod_plot_agg$denominator)
      mod_plot_agg$s  <- 1 / (sqrt(mod_plot_agg$denominator))
      
    }
  } 
  
  if(data_type == "PR"){
    
    # use average proportion as target
    mod_plot_agg$Target<- asin(sum(mod_plot_agg$numerator)/ sum(mod_plot_agg$denominator))
    
    mod_plot_agg$Y <- asin(mod_plot_agg$numerator / mod_plot_agg$denominator)
    mod_plot_agg$s  <- 1 / (2 * sqrt(mod_plot_agg$denominator))
    #mod_plot_agg$rrS2 <- mod_plot_agg$s^2
    
    
  } 
  
  if(data_type=="RC"){
    
    # use average proportion as target
    mod_plot_agg$Target<- log(sum(mod_plot_agg$numerator)/ sum(mod_plot_agg$denominator))
    
    mod_plot_agg$Y <- log((mod_plot_agg$numerator +0.5) / (mod_plot_agg$denominator +0.5))
    mod_plot_agg$s  <- 
      sqrt(
        (mod_plot_agg$numerator/((mod_plot_agg$numerator +0.5)^2))
        /
        (mod_plot_agg$denominator/((mod_plot_agg$denominator +0.5)^2))
      )
                              
    #mod_plot_agg$rrS2 = mod_plot_agg$s^2
    
    
  }
  
  mod_plot_agg$Uzscore <- (mod_plot_agg$Y - mod_plot_agg$Target) / mod_plot_agg$s
  
  return(mod_plot_agg)

}

#' Winsorisation function
#'
#' @description Internal function to perform the winsorization or truncation.
#'
#' @param mod_plot_agg Aggregated model input data
#' @param sr_method Adjustment method for standardised ratios, can take the value \"SHMI\" or \"CQC\". \"SHMI\" is default. Not relevant to PR or RC data types
#' @param Winsorise_by The amount to winsorise\/truncate the distribution by, prior to transformation. 0.1 means 10\% (at each end).
#'
#' @return A data.frame with winsorized z-scores returned added
#' @keywords internal
#' 
#' 
winsorisation <- function(mod_plot_agg = mod_plot_agg, sr_method = "SHMI", Winsorise_by = 0.1){
  
  lz <- quantile(x = mod_plot_agg$Uzscore, Winsorise_by, na.rm = TRUE)
  uz <- quantile(x = mod_plot_agg$Uzscore, (1 - Winsorise_by), na.rm = TRUE)
  mod_plot_agg$Winsorised = ifelse(mod_plot_agg$Uzscore > lz & mod_plot_agg$Uzscore < uz, 0, 1)
  
  if(data_type == "SR" & sr_method == "SHMI"){
    
    #mod_plot_agg$Wuzscore[mod_plot_agg$Winsorised == 1] <- mod_plot_agg$Uzscore
    mod_plot_agg$Wuzscore <-ifelse(mod_plot_agg$Winsorised == 0, mod_plot_agg$Uzscore, NA)
  
  } else {
  
  #if(sr_method == "CQC"){

    mod_plot_agg$Wuzscore = ifelse(mod_plot_agg$Uzscore < lz
                                   , lz
                                   , ifelse(mod_plot_agg$Uzscore > uz
                                          , uz
                                          , mod_plot_agg$Uzscore))
  }
  return(mod_plot_agg)
}



#' Transformation function for z-scoring
#'
#' @description Internal function to perform the transformations for data types.
#'
#' @param n Single numeric value for the count of the number of groups (and therefore z-scores)
#' @param zscores Vector of z-scores z-scores to be used.  Commonly, this would be 'winsorised' first to remove inpact of extreme outliers.  SHMI truncates instead, but this simply reduced the n as well as the z-score.
#'
#' @return A numeric phi value
#' @keywords internal
#' 
#'
phi_func <- function(n, zscores){
  phi <- (1 / n) * sum(zscores^2)
  
  return(phi)
}




#' Calculate the between group standard error (tau2) using a dispersion factor
#'
#' @description Internal function to calcuate teh additional, between group, standard error (tau2) to add to S2.
#'
#' @param n The number of groups for data items, e.g. hospitals trusts that z-scores are calculated at.
#' @param phi The dispersion ratio, where > 1 means overdispersion
#' @param S Standardard error (within cluster, calcualted in z-score process)
#'
#' @return A numeric Tau2 value
#' @keywords internal
#'  
#'  
tau_func <- function(n,  phi, S){

  if((n*phi) < n - 1){
    Tau2 <- 0
  } else {
  
  Tau2 <- max(0, ((sum(n) * sum(phi)) - (sum(n) - 1)) /
          (sum(1/(S^2)) - (sum((1/(S^2))^2) / sum(1/(S^2)))))

  }
  
  return(Tau2)
}



#' Poisson funnel limit calculation
#'
#' @description Adds 95% and 99.8 % funnel limits from Poisson distribution, using eact Poisson limits.
#'
#' @param mod_plot_agg Aggregated model input data
#' @param multiplier Multiplier to adjust limits if reporting by a multiplier, e.g. per 1000.
#' 
#' @return A data.frame of original, aggreagated data plus transformed z-score (unadjusted for overdispersion)
#' @keywords internal
#' 
poisson_limits<-function(mod_plot_agg=mod_plot_agg, multiplier = 1){
  
  mod_plot_agg$LCL95 <- multiplier * mod_plot_agg$Target * (qchisq(0.975, (2*mod_plot_agg$denominator+1), lower.tail = FALSE)/2)/ mod_plot_agg$denominator
  mod_plot_agg$UCL95 <- multiplier * mod_plot_agg$Target * (qchisq(0.025, 2*(mod_plot_agg$denominator), lower.tail = FALSE)/2) / mod_plot_agg$denominator
  mod_plot_agg$LCL99 <- multiplier * mod_plot_agg$Target * (qchisq(0.999, (2*mod_plot_agg$denominator+1), lower.tail = FALSE)/2)/ mod_plot_agg$denominator
  mod_plot_agg$UCL99 <- multiplier * mod_plot_agg$Target * (qchisq(0.999, 2*(mod_plot_agg$denominator), lower.tail = FALSE)/2) / mod_plot_agg$denominator

  return(mod_plot_agg)
}




#' OD funnel limit calculation
#'
#' @description Add 95% and 99.8 % funnel limits from OD-adjusted Poisson distribution
#'
#' @param mod_plot_agg Aggregated model input data
#' @param data_type Type of data for adjustment and plotting: Indirectly Standardised ratio (\"SR\"), proportion (\"PR\"), or ratio of counts (\"RC\").
#' @param sr_method Adjustment method for standardised ratios, can take the value \"SHMI\" or \"CQC\". \"SHMI\" is default. 
#' @param multiplier Multiplier to adjust limits if reporting by a multiplier, e.g. per 1000.
#' @param Tau2 A 'between' standard deviation to add to the within standard deviation, S, to inflate limits.
#'
#' @return A data.frame of with appended OD limits
#' @keywords internal
#' 
OD_limits<-function(mod_plot_agg=mod_plot_agg, data_type = "SR", sr_method = "SHMI", multiplier = 1, Tau2 = 0){
  
  if(data_type == "SR" & sr_method == "SHMI"){
    mod_plot_agg$OD95LCL <- multiplier * (exp(-1.959964 * sqrt((1 / mod_plot_agg$denominator) + Tau2)))
    mod_plot_agg$OD95UCL <- multiplier * (exp(1.959964 * sqrt((1 / mod_plot_agg$denominator) + Tau2)))
    mod_plot_agg$OD99LCL <- multiplier * (exp(-3.090232 * sqrt((1 / mod_plot_agg$denominator) + Tau2)))
    mod_plot_agg$OD99UCL <- multiplier * (exp(3.090232 * sqrt((1 / mod_plot_agg$denominator) + Tau2)))

  } else if(data_type=="RC"){
    
    dfCI$odll95 <- multiplier * ((Target + (-1.959964 * (sqrt( sqrt( 
                                                                    (mod_plot_agg$numerator/((mod_plot_agg$numerator+0.5)^2))
                                                                    +
                                                                    (mod_plot_agg$denominator/((mod_plot_agg$denominator+0.5)^2))
                                                                      ^2) + Tau2))))^2)
    dfCI$odul95 <- multiplier * ((Target + (1.959964 * (sqrt( sqrt( 
                                                                    (mod_plot_agg$numerator/((mod_plot_agg$numerator+0.5)^2))
                                                                    +
                                                                      (mod_plot_agg$denominator/((mod_plot_agg$denominator+0.5)^2))
                                                                    ^2) + Tau2))))^2)
    dfCI$odll998 <- multiplier * ((Target + (-3.090232 * (sqrt( sqrt( 
                                                                    (mod_plot_agg$numerator/((mod_plot_agg$numerator+0.5)^2))
                                                                    +
                                                                      (mod_plot_agg$denominator/((mod_plot_agg$denominator+0.5)^2))
                                                                    ^2) + Tau2))))^2)
    dfCI$odul998 <- multiplier * ((Target + (3.090232 * (sqrt( sqrt( 
                                                                    (mod_plot_agg$numerator/((mod_plot_agg$numerator+0.5)^2))
                                                                    +
                                                                      (mod_plot_agg$denominator/((mod_plot_agg$denominator+0.5)^2))
                                                                    ^2) + Tau2))))^2)
    
  } else {
    
    mod_plot_agg$OD95LCL <-  multiplier * mod_plot_agg$Target * ((1 + (-1.959964 * (sqrt((mod_plot_agg$s^2) + Tau2))))^2)
    mod_plot_agg$OD95UCL <-  multiplier * mod_plot_agg$Target * ((1 + (1.959964 * (sqrt((mod_plot_agg$s^2) + Tau2))))^2)
    mod_plot_agg$OD99LCL <-  multiplier * mod_plot_agg$Target * ((1 + (-3.090232 * (sqrt((mod_plot_agg$s^2) + Tau2))))^2)
    mod_plot_agg$OD99UCL <-  multiplier * mod_plot_agg$Target * ((1 + (3.090232 * (sqrt((mod_plot_agg$s^2) + Tau2))))^2)

  }
  
  return(mod_plot_agg)
}
