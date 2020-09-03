#' @title COnstructor for new funnel plot object
#' @export 
new_funnel_plot <- function(funnelplot = list()) {
  
  stopifnot(is.list(funnelplot))
  
  structure(funnelplot,
            class = "funnelplot")
  
}
