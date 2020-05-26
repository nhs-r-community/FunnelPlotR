library(FunnelPlotR)
library(COUNT)
library(ggplot2)

# Setup
data_type = "SR"
label_outliers = 99
Poisson_limits = FALSE
OD_adjust = TRUE
sr_method = "SHMI"
Winsorise_by = 0.1
title="Untitled Funnel Plot"
multiplier = 1
x_label = "Expected"
y_label = "Standardised Ratio"
xrange = "auto"
yrange = "auto"
return_elements=c("plot", "data", "limits")
theme = funnel_clean()
# lets use the \'medpar\' dataset from the \'COUNT\' package. Little reformatting needed
#library(COUNT)
data(medpar)
medpar$provnum<-factor(medpar$provnum)
medpar$los<-as.numeric(medpar$los)

mod<- glm(los ~ hmo + died + age80 + factor(type), family="poisson", data=medpar)
summary(mod)

# Get predicted value for ratio
medpar$prds<- predict(mod, type="response")

# Draw plot, returning just the plot object
fp2<-funnel_plot(denominator=medpar$prds,numerator=medpar$los, 
                 group = medpar$provnum, return_elements=c("plot"))

fp2


funnel_plot(numerator=medpar$los, denominator=medpar$prds, group = medpar$provnum,
            title = 'Length of Stay Funnel plot for `medpar` data', Poisson_limits = FALSE,
            OD_adjust = TRUE, label_outliers = 99, sr_method="CQC")

fp[[2]]

View(fp[[3]])

fp[[1]]

fp<-readRDS("plot_reference.RDS")
library(gridExtra)

grid.arrange(fp$plot, rtn$plot, nrow=1)


install.packages("C:/Users/Christopher/Documents/R/FunnelPlotR_0.2.9999.tar.gz", repo=NULL)


# Now proportion

funnel_plot(numerator=medpar$los, denominator=(medpar$prds*10), group = medpar$provnum,
            data_type = "RC",return_elements=c("plot"),
            title = 'Length of Stay Funnel plot for `medpar` data', Poisson_limits = FALSE,
            OD_adjust = TRUE, label_outliers = 95, sr_method="SHMI")#, yrange=c(0, 2))

draw_plot(mod_plot_agg, x_label, y_label, title, label_outliers,
                    multiplier=multiplier, Poisson_limits=FALSE, OD_adjust=TRUE,
                    Tau2=Tau2, Target=sum(numerator)/sum(denominator), xrange=xrange, 
                    yrange=yrange, data_type=data_type,
                    sr_method = sr_method, theme = theme)
