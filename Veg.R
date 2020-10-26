library(dplyr)
library(metafor)
make_pct <- function(x) (exp(x) - 1) * 100
#veg <- read.csv("~/OneDrive - LLNL/FACEreview/data/AGB_effects2019.csv",na.strings=c("",NA))
#veg <- veg[complete.cases(veg$id),]
#veg<- dplyr::filter(veg,!is.na(CNr),!is.na(PamountBray09_new)) %>%
 #select(Experiment.Name, SITE, Myc,N,Biome,System.Type,Source.Reference, yi, vi)
#write.csv(veg,"vegetation.csv")
veg <- read.csv("vegetation.csv",stringsAsFactors=TRUE)

dplyr::filter(veg, Biome=="Temperate_Forest", N=="Nlow") %>% group_by(Myc) %>%
  summarise(mean=make_pct(mean(yi)), se=make_pct(sd(yi)/sqrt(n())), n=n())

dplyr::filter(veg, System.Type=="Tree Stand", N=="Nlow") %>% group_by(Myc) %>%
  summarise(mean=make_pct(mean(yi)), se=make_pct(sd(yi)/sqrt(n())), n=n())          

summary(veg_eco<-rma.mv(yi, vi, mods= ~System.Type-1, data=veg,  random = ~ 1 | SITE / X))
make_pct(coef(summary(veg_eco)))
