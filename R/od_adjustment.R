#' Transformation function for z-scoring
#'
#' @description Internal function to perform the transformations for data types.
#'
#' @param mod_plot_agg Aggregated model input data
#' @param data_type Type of data for adjustment and plotting: Indirectly Standardised ratio (\"SR\"), proportion (\"PR\"), or ratio of counts (\"RC\").
#' @param sr_method Adjustment method, can take the value \"SHMI\" or \"CQC\". \"SHMI\" is default. 
#'
#' @return A data.frame of original, aggregated data plus transformed z-score (unadjusted for overdispersion)
#' @keywords internal
#' 
transformed_zscore<-function(mod_plot_agg=mod_plot_agg, data_type = "SR", sr_method = "SHMI"){

  if(data_type == "SR"){
    
    # log-transformed SHMI version
    if(sr_method == "SHMI"){
      mod_plot_agg$target_transformed<- 0
      mod_plot_agg$Y <- log(mod_plot_agg$numerator / mod_plot_agg$denominator)
      mod_plot_agg$s <- 1 / (sqrt(mod_plot_agg$denominator))
      
    # SQRT-transformed CQC version
    } else if(sr_method == "CQC"){
      mod_plot_agg$target_transformed<- 1
      mod_plot_agg$Y <- sqrt(mod_plot_agg$numerator / mod_plot_agg$denominator)
      mod_plot_agg$s  <- 1 / (2 * sqrt(mod_plot_agg$denominator))
      
    }
  } 
  
  if(data_type == "PR"){
    
    # use average proportion as target_transformed
    mod_plot_agg$target_transformed<- asin(sqrt(sum(mod_plot_agg$numerator)/ sum(mod_plot_agg$denominator)))
    
    mod_plot_agg$Y <- asin(sqrt(mod_plot_agg$numerator / mod_plot_agg$denominator))
    mod_plot_agg$s  <- 1 / (2 * sqrt(mod_plot_agg$denominator))
    
    
  } 
  
  if(data_type=="RC"){
    
    # use average proportion as target_transformed
    mod_plot_agg$target_transformed<- log(sum(mod_plot_agg$numerator)/ sum(mod_plot_agg$denominator))
    
    mod_plot_agg$Y <- log((mod_plot_agg$numerator +0.5) / (mod_plot_agg$denominator +0.5))
    mod_plot_agg$s  <- 
      sqrt(
        (mod_plot_agg$numerator/((mod_plot_agg$numerator + 0.5 )^2))
        +
        (mod_plot_agg$denominator/((mod_plot_agg$denominator +0.5)^2))
      )
                              
    #mod_plot_agg$rrS2 = mod_plot_agg$s^2
    
    
  }
  
  mod_plot_agg$Uzscore <- (mod_plot_agg$Y - mod_plot_agg$target_transformed) / mod_plot_agg$s
  
  return(mod_plot_agg)

}


#' Winsorisation function
#'
#' @description Internal function to perform the Winsorisation.
#'
#' @param mod_plot_agg Aggregated model input data
#' @param trim_by The amount to Winsorise the distribution by, prior to transformation. 0.1 means 10\% (at each end).
#'
#' @return A data.frame with winsorised z-scores returned added
#' @keywords internal
#' 
#' 
winsorisation <- function(mod_plot_agg = mod_plot_agg, trim_by = 0.1){
  
  lz <- quantile(x = mod_plot_agg$Uzscore, trim_by, na.rm = TRUE)
  uz <- quantile(x = mod_plot_agg$Uzscore, (1 - trim_by), na.rm = TRUE)
  mod_plot_agg$winsorised = ifelse(mod_plot_agg$Uzscore > lz & mod_plot_agg$Uzscore < uz, 0, 1)
  
  mod_plot_agg$Wuzscore = ifelse(mod_plot_agg$Uzscore < lz
                                   , lz
                                   , ifelse(mod_plot_agg$Uzscore > uz
                                          , uz
                                          , mod_plot_agg$Uzscore))
  
  return(mod_plot_agg)
}

#' Truncation function for NHSD method
#'
#' @description Internal function to perform the truncation.
#'
#' @param mod_plot_agg Aggregated model input data
#' @param trim_by The amount to truncate the distribution by, prior to transformation. 0.1 means 10\% (at each end).
#'
#' @return A data.frame with truncated z-scores added
#' @keywords internal
#' 
#' 
truncation <- function(mod_plot_agg = mod_plot_agg, trim_by = 0.1){
  
  # How many groups for truncation
  k <- 1/trim_by
  maxk<- k-1
  mink<-min(k/k)-1
  
  mod_plot_agg$rk <- rank(mod_plot_agg$Uzscore, ties.method = "average")
  mod_plot_agg$sp <- floor(mod_plot_agg$rk  * k / (length(mod_plot_agg$rk) + 1))
  
  mod_plot_agg$truncated = ifelse(mod_plot_agg$sp > mink & mod_plot_agg$Uzscore < maxk, 0, 1)
  
  mod_plot_agg$Wuzscore = ifelse(mod_plot_agg$truncated == 1 
                                , NA
                                , mod_plot_agg$Uzscore)
  
  return(mod_plot_agg)
}



#' Calculate overdispersion ratio
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
#' @description Internal function to calculate the additional, between group, standard error (tau2) to add to S2.
#'
#' @param n The number of groups for data items, e.g. hospitals trusts that z-scores are calculated at.
#' @param phi The dispersion ratio, where > 1 means overdispersion
#' @param S Standard error (within cluster, calculated in z-score process)
#'
#' @return A numeric Tau2 value
#' @keywords internal
#'  
#'  
tau_func <- function(n,  phi, S){

  if((n*phi) < (n - 1)){
    Tau2 <- 0
  } else {
  
  Tau2 <- max(0, ((sum(n) * sum(phi)) - (sum(n) - 1)) /
          (sum(1/(S^2)) - (sum((1/(S^2))^2) / sum(1/(S^2)))))

  }
  
  return(Tau2)
}

