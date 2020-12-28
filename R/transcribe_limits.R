#' Transcribe names of control limits into test from values supplied to function
#'
#' @param limits_z z-values for limits in question, e.g. 95\% are approx -1.96 & 1.96
#' @param Poisson_limits TRUE or FALSE are we drawing poisson control limits?
#' @param OD_adjust TRUE or FALSE are we drawing overdispersed poisson control limits?
#'
#' @return A vector of names for control limits
#' @export

transcribe_limits <- 
  function(limits_z, Poisson_limits, OD_adjust){
    
    lms <- paste0(as.character(100 * abs((show_limits_working * 2)-1)),"%")
    low_up <- rep(c("Lower", "Upper"), length(lms)/2)
    
    
    if(Poisson_limits){
      if(OD_adjust){
        desc <- c(rep("Poisson",length(lms)/2), rep("Overdispersed",length(lms)/2))
      } else {
        desc <- c(rep("Poisson",length(lms)))
      }
    } else {
      desc <- c(rep("Overdispersed",length(lms)))
    }
    
    paste(lms, low_up, desc)
    
  }
