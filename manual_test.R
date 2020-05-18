library(NHSRdatasets)
library(FunnelPlotR)
library(dplyr)

data("LOS_model")

model<- glm(Death ~ LOS + Age, data=LOS_model)

# Add predictions and write to data base to test database queries
LOS_model$preds <- predict(model, type="response")

mod_plot <-
  LOS_model %>%
  group_by(Organisation) %>%
  summarise(observed = sum(Death),
            predicted = sum(preds)) 

# install.packages("C:/Users/Christopher/Documents/R/FunnelPlotR_0.2.3.tar.gz", repos=NULL)

