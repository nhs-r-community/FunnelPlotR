#' Winsorisation and adjusted z-score function
#'
#' @description Internal function to perform the winsorisation and adjustment of z-scores prior to plotting.
#'
#' @param mod_plot_agg Aggregated model input data
#' @param sr_method Adjustment method, can take the value \"SHMI\" or \"CQC\". \"SHMI\" is default.
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

if (missing(denominator)) {
stop("Need to specify model denominator")}

if(data_type == "SR"){
  
  # sum(observed) = sum(expected in SR model)
  Target<- 1
  
  # log-transformed SHMI verison
  if(sr_method == "sHMI"){}
  
  
  # sQRT-transformed CQC version
  if(sr_method == "CQC"){
  
    mod_plot_agg$Y <- sqrt(mod_plot_agg$numerator / mod_plot_agg$denominator)
    mod_plot_agg$s  <- 1 / (sqrt(mod_plot_agg$denominator))
    mod_plot_agg$rrS2 <- mod_plot_agg$s^2
  }

    
} if(data_type == "PR") {
  
  # use average proportion as target
  Target<- asin(sum(mod_plot_agg$numerator)/ sum(mod_plot_agg$denominator))
  
  mod_plot_agg$Y <- asin(mod_plot_agg$numerator / mod_plot_agg$denominator)
  mod_plot_agg$s  <- 1 / (2 * sqrt(mod_plot_agg$denominator))
  mod_plot_agg$rrS2 <- mod_plot_agg$s^2
  
  
} if(data_type=="RC"){
  
  # use average proportion as target
  Target<- log(sum(mod_plot_agg$numerator)/ sum(mod_plot_agg$denominator))
  
  mod_plot_agg$Y <- log((mod_plot_agg$numerator +0.5) / (mod_plot_agg$denominator +0.5))
  mod_plot_agg$s  <- 
    sqrt(
      (mod_plot_agg$numerator/((mod_plot_agg$numerator +0.5)^2))
      /
      (mod_plot_agg$denominator/((mod_plot_agg$denominator +0.5)^2))
    )
                            
  mod_plot_agg$rrS2 = mod_plot_agg$s^2
  
  
} else {stop("Invalid adjustment method supplied in 'data_type' argument")}

mod_plot_agg$Uzscore <- (mod_plot_agg$Y - mod_plot_agg$Target) / mod_plot_agg$s

return(mod_plot_agg)

}

#' Winsorisation function
#'
#' @description Internal function to perform the winsorization or truncation.
#'
#' @param mod_plot_agg Aggregated model input data
#' @param data_type Type of data for adjustment and plotting: Indirectly Standardised ratio (\"SR\"), proportion (\"PR\"), or ratio of counts (\"RC\").
#' @param sr_method Adjustment method, can take the value \"SHMI\" or \"CQC\". \"SHMI\" is default.
#' @param Winsorise_by The amount to winsorise\/truncate the distribution by, prior to transformation. 0.1 means 10\% (at each end).
#'
#' @return A data.frame with winsorized z-scores returned added
#' @keywords internal
#' 
#' 
winsorisation <- function(mod_plot_agg = mod_plot_agg, sr_method = "SHMI"){
  
  lz <- quantile(x = mod_plot_agg$Uzscore, Winsorise_by, na.rm = TRUE)
  uz <- quantile(x = mod_plot_agg$Uzscore, (1 - Winsorise_by), na.rm = TRUE)
  mod_plot_agg$Winsorised = ifelse(mod_plot_agg$Uzscore > lz & mod_plot_agg$Uzscore < uz, 0, 1)
  
  if(sr_method == "SHMI"){
    
    mod_plot_agg$Wuzscore[mod_plot_agg$Winsorised == 1] <- mod_plot_agg$Uzscore
  
    
  } if(sr_method == "CQC"){

    mod_plot_agg$Wuzscore = ifelse(mod_plot_agg$Uzscore < lz
                                   , lz
                                   , ifelse(mod_plot_agg$Uzscore > uz
                                          , uz
                                          , mod_plot_agg$Uzscore))
   
  }
  
  mod_plot_agg$Wuzscore2 = mod_plot_agg$Wuzscore^2

  return(mod_plot_agg)
}


 