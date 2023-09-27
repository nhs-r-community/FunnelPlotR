

#' Test function
#'
#' @param .data data frame
#' @param numerator tghe numearto
#' @param denominator denom
#' @param group the group
#'
#' @return a data frame stuck together
#' @export
#' 
#' @importFrom rlang .data
#'
tst_func <- function(.data, numerator, denominator, group){
  
  numerator <- quo_name(enquo(numerator))
  denominator <- quo_name(enquo(denominator))
  group <- quo_name(enquo(group))
  
  
  
  mod_plot <- data.frame(numerator=as.numeric(.data[[numerator]])
                         ,denominator=as.numeric(.data[[denominator]])
                         , group=as.factor(.data[[group]]))
  
  return(mod_plot)
  
}


library(COUNT)
data(medpar)

medpar$provnum<-factor(medpar$provnum)
medpar$los<-as.numeric(medpar$los)

mod<- glm(los ~ hmo + died + age80 + factor(type), family="poisson", data=medpar)
summary(mod)

# Get predicted value for ratio
medpar$prds<- predict(mod, type="response")

tst_func(medpar, los, prds, provnum)

funnel_plot(medpar, los, prds, provnum)
