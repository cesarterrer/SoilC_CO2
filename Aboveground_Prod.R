source("/Users/terrermoreno1/Documents/SOC-prod/1. Effect Size.R")
library(ggplot2)
library(cowplot)
library(ggrepel)
library(rcartocolor)
library(dplyr)
library(metafor)
theme_set(theme_cowplot(font_size=5))
options(bitmapType="cairo")
# display_carto_all()
display_carto_pal(12, "Bold")
mycols <- carto_pal(12, "Bold")[c(4,3,2,9)]
mycols2 <- carto_pal(12, "Bold")[c(7,8)]
mycols3 <- carto_pal(12, "Bold")[c(1,3)]
mycols_vegsoil <- carto_pal(12, "Bold")[c(2,4)]
make_pct <- function(x) (exp(x) - 1) * 100

dat <- read.csv("/Users/terrermoreno1/Documents/SOC-prod/soilC_prod_meta.csv") %>%
  filter(biomass != "NA", # Remove experiments with missing biomass data
              nyears >= 0.5, # Remove experiments of less than 6 months duration
              biomass.type == "ABI" |biomass.type == "ANPP" |
              biomass.type == "foliage" | biomass.type == "NPP" | biomass.type == "yield") %>% 
  mutate(obs= 1:nrow(.), 
         Myc= recode(Myc, Nfixer = "N-fixer"), 
         N=recode(N,Nlow = "non-fertilized",
                  Nhigh = "N-fertilized"))
write.csv(dat, "SOC_Prod.csv")
dplyr::select(dat,Experiment,Experiment_type,Ecosystem.type,nyears,Citation) %>% write.csv("metadata.csv")
filtered <- filter(dat, N=="non-fertilized", Experiment_type != "Chamber", Disturbance=="intact")
fertilized <- filter(dat, N=="N-fertilized", Experiment_type != "Chamber", Disturbance=="intact")
intact <- filter(dat, Experiment_type != "Chamber", Disturbance=="intact")
## BIOMASS ##
summary(intact.m<-rma.mv(yi, vi, mods= ~biomass*N + I(biomass^2)*N, data=intact,  random = ~ 1 | Site / obs)) # Significant interaction
summary(fert.m<-rma.mv(yi, vi, mods= ~biomass + I(biomass^2), data=fertilized, random = ~ 1 | Site / obs))
anova(fert.m)
summary(natural.m<-rma.mv(yi, vi, mods= ~biomass + I(biomass^2), data=filtered, random = ~ 1 | Site / obs)) # In natural soils
anova(natural.m)
natural0.m<-rma.mv(yi, vi, data=filtered, random = ~ 1 | Site / obs)
(sum(natural0.m$sigma2) - sum(natural.m$sigma2)) / sum(natural0.m$sigma2) #R2
# UNFERTILIZED
Brange <- seq(min(filtered$biomass), max(filtered$biomass), .001)
unfert.new <- data.frame(biomass = Brange, N=factor("non-fertilized", levels=c("N-fertilized","non-fertilized")))
unfert.mods <- model.matrix(~ biomass*N + I(biomass^2)*N, unfert.new)[,-1]
unfert.pred <- as.data.frame(predict(intact.m, newmods = unfert.mods, addx=T, transf=make_pct))

p1 <- ggplot(unfert.pred, aes(make_pct(X.biomass), pred)) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_vline(xintercept = 0, lty=2, size=1) + theme_classic() + 
  geom_point(data=filtered,alpha=0.7,
             col=carto_pal(12, "Bold")[3], show.legend = FALSE,
             aes(y=make_pct(yi), x=make_pct(biomass), col=Myc,
                 size=1/(natural.m$tau2+vi))) + 
  #geom_point(colour="red",data=fertilized,aes(make_pct(biomass), make_pct(yi)))+
  geom_line (size=0.8) + 
  #geom_line (data=predsfert,aes(make_pct(X.rcs.biomass..knots.biomass), pred), size=0.8, col="red") + 
  geom_ribbon(aes(ymax= ci.ub, ymin=ci.lb), alpha=0.1) +
  labs(x=expression(paste(CO[2]," effect on aboveground biomass production (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  scale_size(range = c(1, 5)) +
  guides(size=FALSE,col = guide_legend(title = NULL)) + 
  theme_cowplot(font_size=8)
p1

save_plot("ANPP_regression.png",p1, type = "cairo-png",base_aspect_ratio = 1.3)

source("litter.R")

multi <- plot_grid(lit_prod + theme(plot.margin = unit(c(10,5,5,5), "points")), 
                        p1 + theme(plot.margin = unit(c(10,10,5,5), "points")),
                        align="hv",
                        labels = "auto",
                        hjust = -.1,
                        vjust = 1.2,
                        rel_widths = c(.7, 1),
                        nrow = 1, ncol=2)
save_plot("graphs/litter_prod_regression.png",multi, ncol=2, nrow=1, dpi= 1200, base_height = 3, base_width = 3, type = "cairo-png")
