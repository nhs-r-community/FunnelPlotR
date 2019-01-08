

#'
#' This is a basic implementation of Speigelhalter's funnel plots for using wiht Chris Mainey's PhD work, using
#' aggregated incident reporting data from NRLS.  It expects a specific structure of table and column names.
#'
#' @title Funnel Plots for Incident Reporting: A function for generating funnel plots or incident reporting data

#'
#' @details There are a number of parameters for the input, with the assumption that
#' @details you will want smooth funnel limits plotted based on the DerSimmonian Laird Tau2 additive random
#' effects limits.
#' @details To use exact (with not limits outside of data range), set Exac_limits=TRUE. To use standard
#' 2 & 3 sigma limits,
#' @details set OD_Tau2 to FALSE.  Outliers are marked based on Trust code.
#'
#' @param model The name of the model being plotted (lm, glm, lmer, glmer, mgcv::gam, glmmTMB). Alternatively, a vector of fitted values.
#' @param data dataframe of the source data for the plot.
#' @param observed_name  name of the observed count column in dataframe.
#' @param OD_Tau2 Draw overdispersed limit, using Speigelhalter's Tau2 (default=TRUE).
#' @param Exact_limits Draw exact limits based only on data points with no iterpolation. (default=FALSE).
#' @param label_outliers Add trust codes to outlier organisations on plot. (default=TRUE).
#' @param reportingyr Reporting year, if using more than one fiscal year, specify in format: '2015/16'.
#' Ignored if not specified.
#' @param title specify title for funnel plot, or default one is constructed from dataname.
#' @param conditional Predict lmer/glmer or GAMs with random effects as conditional? Default is FALSE, prediction is marginal
#' i.e. without random effect for organisation.
#' @return a list of the funnel plot as GGPlot object, the limits table and the base table for plot.
#' @examples funnel_plot(Pois_count_1, incsub2a_st, 'incidents', '2015/16', 'Poisson model')
#' funnel_plot(Quasi_count_1, incsub2a_st, 'incidents', '2015/16', 'Quasi model')
#' funnel_plot(NB_count_1, incsub2a_st, 'incidents', OD_Tau2=FALSE, Exact_limits=FALSE, label_outliers=FALSE, '2015/16', 'NegBin model')
#' funnel_plot(Pois_count_1_rint, incsub2a_st, 'incidents', OD_Tau2=TRUE, Exact_limits=TRUE, label_outliers=TRUE,'2015/16', 'Poisson Rint model')
#' @export



funnel_plot<-function(model, data, observed_name, OD_Tau2=TRUE, Exact_limits=FALSE,
                      label_outliers=TRUE, reportingyr, title, conditional=FALSE, ...){

  require(lme4)
  require(splines)
  require(mgcv)
  require(ggplot2)
  require(ggrepel)
  require(glmmTMB)
  require(scales)
  require(MASS)
  require(dplyr)
  require(randomForest)
  #build inital dataframe of obs/prdicted, with error message caught here in 'try'

  if(missing(model))
    stop("Need to specify a model to build from")
  if(missing(data))
    stop("Need to supply a data frame source")
  if(missing(title))
    title<-paste("Funnel plot of", quote(model), "for", quote(data))
  if(missing(observed_name))
    stop("Need to supply the column name for observed events")

  if(class(model)[1]  == "numeric"){
      pr<-model
  } else if(class(model)[1]  == "array"){
    pr<-as.numeric(model)
  } else  if(class(model)[1]  == "randomForest.formula" | class(model)[1]  == "randomForest" ){
     pr<-predict(model, newdata=data)
  } else if(class(model)[1]  == "glmerMod" | class(model)[1]  == "lmerMod"){
    if(conditional == TRUE) {
      pr<-predict(model, newdata=data, type="response")
    } else {
      pr<-predict(model, newdata=data, type="response", re.form=NA)
    }
  } else if(class(model)[1]  == "gam"){
    if(conditional ==TRUE){
      pr<-predict(model, newdata=data, type="response")
    } else {
        pr<-predict(model, newdata=data, type="response", exclude="s(org_code)")
    }
  } else if(class(model)[1]  == "glmmTMB" | class(model)[1]  == "glm" | class(model)[1]  == "lm"){
      pr<-predict(model, newdata=data, type="response")
  } else {
    stop("Unknown model class. Specify lm/glm/lmer/glmer/glmTMB/mgcv:gam classes or vector of predictions on response scale")
  }


  if(missing(reportingyr)) {

    mod_plot<-data.frame(
      dplyr::select(data, observed_name),
      pr,
      dplyr::select(data, org_code),
      reportingyr=dplyr::select(data, finyr)
    )
    reportingyr<-mod_plot$reportingyr[1]
    } else {
    mod_plot<-data.frame(
      dplyr::filter(data, finyr==reportingyr) %>% dplyr::select(observed_name),
      pr,
      dplyr::filter(data, finyr==reportingyr) %>% dplyr::select(org_code),
      reportingyr
    )   }

  colnames(mod_plot) <- c("obs", "preds", "org_code","finyr")

  mod_plot_agg<- mod_plot %>% dplyr::group_by(org_code) %>% dplyr::summarise(observed=as.numeric(sum(obs)),
                                                                      predicted=as.numeric(sum(preds)),
                                                                      rr=observed/predicted,
                                                                      rrS2= 1/predicted) %>%
    dplyr::mutate(Uzscore=sqrt(predicted)*log(observed/predicted),
           LCI2=exp(-2 * sqrt(rrS2)),
           UCI2=exp(2 * sqrt(rrS2)),
           LCI3=exp(-3 * sqrt(rrS2)),
           UCI3=exp(3 * sqrt(rrS2)))


  #mod_plot_agg

  #Determine the range of plots
  max_preds<-dplyr::summarise(mod_plot_agg, max(predicted)) %>% ceiling() %>% as.numeric()
  min_ratio<-dplyr::summarise(mod_plot_agg, min(observed/predicted)) %>% as.numeric()
  max_ratio<-dplyr::summarise(mod_plot_agg, max(observed/predicted)) %>% as.numeric()


  ##Winsorisation

  lz=quantile(x = mod_plot_agg$Uzscore, 0.1)
  uz=quantile(x = mod_plot_agg$Uzscore, 0.9)

  mod_plot_agg<-mod_plot_agg %>% dplyr::mutate( Winsorised = ifelse(Uzscore>=lz & Uzscore<=uz, 0, 1),
                                         Wuzscore2= ifelse(Uzscore<lz, lz, ifelse(Uzscore>uz,lz,Uzscore)) ^2,
                                         Tau2= ((sum(1 - Winsorised) * sum(ifelse(Winsorised==0,Wuzscore2,0)) / sum(1 - Winsorised))
                                                - (sum(1 - Winsorised) - 1)) / (sum(ifelse(Winsorised==0,predicted,0)) -
                                                                                  (sum(ifelse(Winsorised==0, predicted * predicted,0)) /
                                                                                   sum(ifelse(Winsorised==0,predicted,0)))),
                                         ODLCI= ifelse(Tau2<=0, exp(-2 * sqrt(rrS2)),  exp(-2 * sqrt(rrS2+Tau2))),
                                         ODUCI= ifelse(Tau2<=0, exp(2 * sqrt(rrS2)),  exp(2 * sqrt(rrS2+Tau2))),
                                         OD3LCI= ifelse(Tau2<=0, exp(-3 * sqrt(rrS2)),  exp(-3 * sqrt(rrS2+Tau2))),
                                         OD3UCI= ifelse(Tau2<=0, exp(3 * sqrt(rrS2)),  exp(3 * sqrt(rrS2+Tau2)))
  )





    #general limits + Tau2
  set.seed(1)
  number.seq <- seq(1, ceiling(max_preds), 1)
  if(mod_plot_agg$Tau2[1]<=0 || OD_Tau2==FALSE){
      dfCI<- data.frame(
        number.seq,
        ll95 = (qchisq(0.975,2 * number.seq, lower.tail = FALSE) /2)/number.seq,
        ul95 =  (qchisq(0.025,2 * (number.seq+1), lower.tail = FALSE) /2)/number.seq,
        ll999 = (qchisq(0.999,2 * number.seq, lower.tail = FALSE) /2)/number.seq,
        ul999 = (qchisq(0.001,2 * (number.seq+1), lower.tail = FALSE) /2)/number.seq

      )


  if(Exact_limits==FALSE){
    funnel_p<-ggplot(mod_plot_agg, aes(y=(observed/predicted), x=predicted))+
      geom_point(size=1.5) +
      geom_line(aes(x = number.seq, y = ll95, col="95% Poisson"), size = 1, linetype=2, data = dfCI) +
      geom_line(aes(x = number.seq, y = ul95, col="95% Poisson"),  size = 1, linetype=2, data = dfCI) +
      geom_line(aes(x = number.seq, y = ll999, col="99.9% Poisson"), size = 1, data = dfCI) +
      geom_line(aes(x = number.seq, y = ul999, col="99.9% Poisson"),  size = 1,data = dfCI) +
      scale_color_manual(values=c("99.9% Poisson"="#6EDAF0",
                                  "95% Poisson"= "#E3B126"
                                  #"95% Over-dispersed"="dodgerblue2",c(, , "#FFFFFF")
                                  #"99.9% Over-dispersed"="darkorchid"
      ),name="Control limits")+
      scale_y_continuous(limits = c((min_ratio-0.1), (max_ratio+0.1)))+
      #scale_x_continuous(labels = comma, limits = c(0,max_preds+1)) +
      geom_hline(aes(yintercept = 1), linetype=2)+
      xlab("Predicted Incident Reports") +
      ylab("Reporting Ratio (Observed/Predicted)") +
      ggtitle("Funnel Plot of Incident reporting ratio", subtitle = paste(title, "for", reportingyr))+
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5,face = "italic"))+

      guides(colour = guide_legend(title.theme = element_text(
        size = 10,
        face = "bold",
        colour = "black",
        angle = 0)))


    if (label_outliers==TRUE){
      funnel_p<-funnel_p +
        geom_text_repel(aes(label=ifelse(observed/predicted>UCI3,as.character(org_code),'')),hjust=0,vjust=0, size=2.7, nudge_y = 0.01)+
        geom_text_repel(aes(label=ifelse(observed/predicted<LCI3,as.character(org_code),'')),hjust=0,vjust=0, size=2.7, nudge_y = -0.05)
    }


  } else{
    funnel_p<-ggplot(mod_plot_agg, aes(y=(observed/predicted), x=predicted))+
      geom_point(size=1.5) +
      geom_line(aes(x=predicted, y=LCI2, col="95% Poisson"), size = 1, linetype=2)+
      geom_line(aes(x=predicted, y=UCI2, col="95% Poisson"), size = 1, linetype=2)+
      geom_line(aes(x=predicted, y=LCI3, col="99.9% Poisson"), size=1)+
      geom_line(aes(x=predicted, y=UCI3, col="99.9% Poisson"), size=1)+
      scale_color_manual(values=c("99.9% Poisson"="#6EDAF0",
                                  "95% Poisson"= "#E3B126"
                                  #"95% Overdispersed"="dodgerblue2",
                                  #"99.9% Overdispersed"="darkorchid"
      ),name="Control limits")+
      scale_y_continuous(limits = c((min_ratio-0.1), (max_ratio+0.1)))+
      scale_x_continuous(labels = comma, limits = c(0,max_preds)) +

      geom_hline(aes(yintercept = 1), linetype=2)+
      xlab("Predicted Incident Reports") +
      ylab("Reporting Ratio (Observed/Predicted)") +
      #guides(title="Control Limits")+
      ggtitle("Funnel Plot of Incident reporting ratio", subtitle = paste(title, "for", reportingyr))+
      theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5,face = "italic"))+
      guides(colour = guide_legend(title.theme = element_text(
        size = 10,
        face = "bold",
        colour = "black",
        angle = 0)))

    if (label_outliers==TRUE){
      funnel_p<-funnel_p +
        geom_text_repel(aes(label=ifelse(observed/predicted>UCI3,as.character(org_code),'')),hjust=0,vjust=0, size=2.7, nudge_y = 0.01)+
        geom_text_repel(aes(label=ifelse(observed/predicted<LCI3,as.character(org_code),'')),hjust=0,vjust=0, size=2.7, nudge_y = -0.05)
      }
    }


}  else {
      dfCI<- data.frame(
        number.seq,
        odll95 = exp(-2 * sqrt((1/number.seq) + mod_plot_agg$Tau2[1])),
        odul95 = exp(2 * sqrt((1/number.seq) + mod_plot_agg$Tau2[1])),
        odll999 = exp(-3 * sqrt((1/number.seq) + mod_plot_agg$Tau2[1])),
        odul999 = exp(3 * sqrt((1/number.seq) + mod_plot_agg$Tau2[1]))
      )


    #dfCI
    #return(funnel_data)
    if(Exact_limits==FALSE){
      funnel_p<-ggplot(mod_plot_agg, aes(y=(observed/predicted), x=predicted))+
        geom_point(size=1.5) +
        geom_line(aes(x = number.seq, y = odll95, col="95% Overdispersed"), size = 1, linetype=2, data = dfCI) +
        geom_line(aes(x = number.seq, y = odul95, col="95% Overdispersed"),  size = 1, linetype=2, data = dfCI) +
        geom_line(aes(x = number.seq, y = odll999, col="99.9% Overdispersed"), size = 1, data = dfCI) +
        geom_line(aes(x = number.seq, y = odul999, col="99.9% Overdispersed"),  size = 1,data = dfCI) +
        scale_color_manual(values=c("99.9% Overdispersed"="#6EDAF0",
                                    "95% Overdispersed"= "#E3B126"
                                    #"95% Over-dispersed"="dodgerblue2",
                                    #"99.9% Over-dispersed"="darkorchid"
        ),name="Control limits")+
        scale_y_continuous(limits = c((min_ratio-0.1), (max_ratio+0.1)))+
        scale_x_continuous(labels = comma, limits = c(0,max_preds)) +
        geom_hline(aes(yintercept = 1), linetype=2)+
        xlab("Predicted Incident Reports") +
        ylab("Reporting Ratio (Observed/Predicted)") +
        ggtitle("Funnel Plot of Incident reporting ratio", subtitle = paste(title, "for", reportingyr))+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5,face = "italic"))+
        guides(colour = guide_legend(title.theme = element_text(
          size = 10,
          face = "bold",
          colour = "black",
          angle = 0)))

      if (label_outliers==TRUE){
        funnel_p<-funnel_p +
          geom_text_repel(aes(label=ifelse(observed/predicted>OD3UCI,as.character(org_code),'')),hjust=0,vjust=0, size=2.7, nudge_y = 0.01)+
          geom_text_repel(aes(label=ifelse(observed/predicted<OD3LCI,as.character(org_code),'')),hjust=0,vjust=0, size=2.7, nudge_y = -0.05)

      } } else {
        funnel_p<-ggplot(mod_plot_agg, aes(y=(observed/predicted), x=predicted))+
          geom_point(size=1.5) +
          geom_line(aes(x=predicted, y=ODLCI, col="95% Over-dispersed"), size = 1, linetype=2)+
          geom_line(aes(x=predicted, y=ODUCI, col="95% Over-dispersed"), size = 1, linetype=2)+
          geom_line(aes(x=predicted, y=OD3LCI, col="99.9% Over-dispersed"), size = 1)+
          geom_line(aes(x=predicted, y=OD3UCI, col="99.9% Over-dispersed"), size = 1)+
          scale_color_manual(values=c("99.9% Over-dispersed"="#6EDAF0",
                                      "95% Over-dispersed"= "#E3B126"
                                      #"95% Over-dispersed"="dodgerblue2",
                                      #"99.9% Over-dispersed"="darkorchid"
          ),name="Control limits")+
          scale_y_continuous(limits = c((min_ratio-0.1), (max_ratio+0.1)))+
          scale_x_continuous(labels = comma, limits = c(0,max_preds)) +
          geom_hline(aes(yintercept = 1), linetype=2)+
          xlab("Predicted Incident Reports") +
          ylab("Reporting Ratio (Observed/Predicted)") +
          ggtitle("Funnel Plot of Incident reporting ratio", subtitle = paste(title, "for", reportingyr))+
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5,face = "italic"))+
          guides(colour = guide_legend(title.theme = element_text(
            size = 10,
            face = "bold",
            colour = "black",
            angle = 0)))

        if (label_outliers==TRUE){
          funnel_p<-funnel_p +
            geom_text_repel(aes(label=ifelse(observed/predicted>OD3UCI,as.character(org_code),'')),hjust=0,vjust=0, size=2.7, nudge_y = 0.01)+
            geom_text_repel(aes(label=ifelse(observed/predicted<OD3LCI,as.character(org_code),'')),hjust=0,vjust=0, size=2.7, nudge_y = -0.05)
        }

      }

  }



  if(OD_Tau2 == TRUE && mod_plot_agg$Tau2[1]<=0){
    message("No overdispersion detected, using Poisson limits")
    }
  return(list(mod_plot_agg, dfCI, funnel_p))

}



