library(FunnelPlotR)

library(NHSRdatasets)

data(LOS_model)

mod1<- glm(Death ~ Age * LOS, data=LOS_model, family="binomial")

LOS_model$preds <- predict(mod1, type="response")


funnel_plot(LOS_model$preds, LOS_model$Death, LOS_model$Organisation)
predictions<-LOS_model$preds
observed<- LOS_model$Death
group <-LOS_model$Organisation
group <-LOS_model$Organisation

?funnel_plot

library(COUNT)

data(medpar)

write.csv(medpar, "medpar.csv")
medpar<-read.csv("medpar.csv")
medpar$provnum<-as.factor(medpar$provnum)
medpar$type<-as.factor(medpar$type)

medpar <- as.data.frame(medpar)

mod2<- glm(died ~ los + hmo + white + factor(type) + age80, data=medpar, family="binomial")
summary(mod2)

mod3<- glm(died ~ los + age80 +  + factor(type), data=medpar, family="binomial")
summary(mod3)


medpar$preds <- predict(mod3, type="response")


funnel_plot(medpar$preds, medpar$died, medpar$provnum)

predictions<- medpar$preds
observed <- medpar$died
group<- factor(medpar$provnum)


funnel_p + theme_minimal() +
  theme(title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="#9E5400", colour="#9E5400"),
        legend.position = "none")

library(hexSticker)
library(ggedit)
library(showtext)

ggsave(filename = "plot1.png",
funnel_p +
  theme_minimal() +

  theme(title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_blank(),
        plot.background = element_rect(fill= "#D69137", colour="#D69137"),
        panel.background = element_rect(fill="#D69137", colour="#D69137"),
  )
, scale = 0.45, dpi = 400)

sticker(
  funnel_p +
    theme_minimal() +

    theme(title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none",
          axis.text = element_blank()
          )
, package="FunnelPlotR", s_height = 0.8, s_width=0.8,
s_x = 1, s_y = 1,  p_size = 10,
h_fill="#FAFAD0B9", h_color="#6BB8D6CA")
"#ADE4FFCF"


font_add_google("Playfair Display", "playfair")

??fonts
showtext_begin()
font_import()
loadfonts(device = "win")

sticker(
 subplot= "plot1.png"
  , package="FunnelPlotR", s_height = 0.90, s_width=0.9, p_family=windowsFont("Roboto"),
  p_color="white",
  s_x = 1, s_y = 1,  p_size = 17,
  h_fill="#D69137", h_color="#B06502", white_around_sticker=TRUE, dpi=400)
)
c("#FFB85C", )
?hexSticker::load_font()

pdf("Fonts.pdf")
print(as.data.frame(fonttable()), max = NULL)
dev.off()

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Nokora", "nokora",repo = "https://github.com/google/fonts/tree/master/apache")
## Automatically use showtext to render text for future devices
showtext_auto()

extrafont::fonts()

## use the ggplot2 example
sticker(p, package="hexSticker", p_size=8, s_x=1, s_y=.75, s_width=1.3, s_height=1,
        p_family = "gochi", filename="inst/figures/ggplot2-google-font.png")

extrafont::loadfonts(device="win")
ft<-fonttable()

write.csv(ft, "ft.csv")
extrafont::font_import("D:/Fonts/", pattern = "RobotoCondensed")


install_github("Rttf2pt1", "wch")
install_github("extrafont", "wch")