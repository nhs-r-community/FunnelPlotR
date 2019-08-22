library(FunnelPlotR)
library(COUNT)
library(ggplot2)

# lets use the \'medpar\' dataset from the \'COUNT\' package. Little reformatting needed
data(medpar)
medpar$provnum<-factor(medpar$provnum)
medpar$los<-as.numeric(medpar$los)

mod<- glm(los ~ hmo + died + age80 + factor(type), family="poisson", data=medpar)
summary(mod)

medpar$prds<- predict(mod, type="response")

aggregate_input_data=TRUE
label_outliers = 99
Poisson_limits = FALSE
OD_adjust = TRUE
method = "SHMI"
Winsorize_by = 0.1
multiplier = 1
x_label = "Expected"
y_label = "Standardised Ratio"
title="Untitle Funnel Plot"
rm(return_elements)

funnel_plot_dev(denominator=medpar$prds,numerator=medpar$los, group = medpar$provnum, return_elements=c("plot"))
