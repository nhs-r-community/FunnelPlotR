#' @title Constructor for new funnel plot object
#' @export 
new_funnel_plot <- function(funnelplot = list()) {
  
  stopifnot(is.list(funnelplot))

  structure(funnelplot,
            class = "funnelplot")
  
}



#' @title Validator for new funnel plot object
#' @export 
validate_funnel_plot <- function(funnelplot){
  
  if (!is.list(funnelplot[[1]])){
    stop(
      "Invalid ggplot object"
    )
  }
  if (!is.data.frame(funnelplot[[2]])){
    stop(
      "Invalid limits data.frame"
    )
  }
  if (!is.data.frame(funnelplot[[3]])){
    stop(
      "Invalid aggregate date.frame"
    )
  }
  if (!is.numeric(funnelplot[[4]])){
    stop(
      "Invalid phi value, requires double()"
    )
  }
  if (!is.numeric(funnelplot[[5]])){
    stop(
      "Invalid tau2 value, requires double()"
    )
  }
  if (!is.logical(funnelplot[[6]])){
    stop(
      "Invalid OD_adjust value.  Expects logical."
    )
  }
  if (!is.logical(funnelplot[[7]])){
    stop(
      "Invalid Poisson_limits value.  Expects logical."
    )
  }
  if (!is.data.frame(funnelplot[[8]])){
    stop(
      "Invalid outliers date.frame"
    )
  }
}



#### Methods


limits <- function(x) {
  UseMethod("limits")
}


# plot
# data
# limits
# phi
# tau
# outliers
