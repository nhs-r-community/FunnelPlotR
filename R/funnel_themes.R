#' A clean funnel plot theme
#' @description A ggplot theme function for clean looking funnel plots.  Try funnel_grey if you like the old one.
#' @return a list of ggplot theme items
#'
#' @seealso funnel_grey
#' 
#' @importFrom ggplot2 theme_minimal theme_bw theme theme_grey
#' @export
#' 
#' @examples funnel_plot(theme=funnel_clean())
funnel_clean <- function(){
  theme_minimal() %+replace% 
  theme(legend.position = "bottom")
}


#' A grey ggplot funnel theme
#' @description A calssic ggplot theme function for funnel plots.  Try funnel_clean if you don't like the grey background.
#' @return a list of ggplot theme items
#' 
#' @seealso funnel_clean
#'
#' @importFrom ggplot2 theme_classic
#' @export
#'
#' @examples funnel_plot(theme=funnel_grey())
funnel_grey <- function(){
  theme_grey()
}
