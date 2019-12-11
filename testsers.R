a<-2
b<-2

if(a==1 | a==2 & b==1){
  print("yes")
} else print("no")


number.seq<-seq(5,50,5)

t <- 0.5

t * ((qchisq(0.975, 2 * number.seq, lower.tail = FALSE) / 2) / number.seq)



funnel_plot(numerator = c(8, 15,18,8,12, 22), denominator= c(12, 21, 26,15,20, 30), OD_adjust=FALSE,
               group = factor(c("a","b","c", "d","e","f")), type = "pr", return_elements = "plot")
)



mod_plot<-data.frame(numerator = c(8, 15,18,8,12, 22), denominator= c(12, 21, 26,15,20, 30),
               group = factor(c("a","b","c", "d","e","f")))

mod_plot_agg<-aggregate_func(mod_plot)
  
aggregate_func<- function(mod_plot) {
  
  mod_plot_agg <- mod_plot %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarise(
      numerator = as.numeric(sum(.data$numerator)),
      denominator = as.numeric(sum(.data$denominator)),
      ratio = .data$numerator / .data$denominator
    )
  
  return(mod_plot_agg)
  
}

