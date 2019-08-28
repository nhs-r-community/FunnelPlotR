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
fp<-funnel_plot(denominator=medpar$prds,method="CQC",
                    numerator=medpar$los, group = medpar$provnum)

fp


