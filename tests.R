library(FunnelPlotR)
library(COUNT)
library(ggplot2)

# lets use the \'medpar\' dataset from the \'COUNT\' package. Little reformatting needed
library(COUNT)
data(medpar)
medpar$provnum<-factor(medpar$provnum)
medpar$los<-as.numeric(medpar$los)

mod<- glm(los ~ hmo + died + age80 + factor(type), family="poisson", data=medpar)
summary(mod)

# Get predicted value for ratio
medpar$prds<- predict(mod, type="response")

# Draw plot, returning just the plot object
fp<-funnel_plot(denominator=medpar$prds,numerator=medpar$los, group = medpar$provnum, return_elements=c("plot"))

fp<-


funnel_plot(numerator=medpar$los, denominator=medpar$prds, group = medpar$provnum
,
            title = 'Length of Stay Funnel plot for `medpar` data', Poisson_limits = FALSE,
            OD_adjust = TRUE, label_outliers = 99, method="SHMI")

fp[[2]]

View(fp[[3]])
