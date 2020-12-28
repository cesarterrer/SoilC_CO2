source("1. Effect Size.R")
library(ggplot2)
library(cowplot)
library(ggrepel)
library(rcartocolor)
theme_set(theme_cowplot(font_size=5))
options(bitmapType="cairo")
# display_carto_all()
display_carto_pal(12, "Bold")
mycols <- carto_pal(12, "Bold")[c(4,3,2,9)]
mycols2 <- carto_pal(12, "Bold")[c(7,8)]
mycols3 <- carto_pal(12, "Bold")[c(1,3)]
mycols_vegsoil <- carto_pal(12, "Bold")[c(2,4)]
make_pct <- function(x) (exp(x) - 1) * 100

dat <- dat %>% mutate(Myc= recode(Myc, Nfixer = "N-fixer"), N=recode(N,Nlow = "non-fertilized",Nhigh = "N-fertilized"))
dplyr::select(dat,Experiment,Experiment_type,Ecosystem.type,nyears,Citation) %>% write.csv("metadata.csv")
# New weights including the number of years of the experiment (deGraaff et a. 2006; van Groenigen 2006)
dat$weightsTime <- with(dat, ((amb.n * elev.n)/(amb.n + elev.n)) + ((nyears^2)/(2*nyears)))
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
  labs(x=expression(paste(CO[2]," effect on plant biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  scale_size(range = c(1, 5)) +
  guides(size=FALSE,col = guide_legend(title = NULL)) + 
  #scale_x_continuous(breaks = seq(-15, 75, by = 15), limits = c(-15,75)) +
  #scale_y_continuous(breaks = seq(-20, 40, by = 10), limits = c(-23,40)) +
  #geom_text_repel(data=filtered, aes(y=make_pct(yi), x=make_pct(biomass),label=paste(Experiment,Myc))) +
  theme_cowplot()
p1
#save_plot("graphs/regression.png",p1, type = "cairo-png",base_aspect_ratio = 1.3)

# FERTILIZED
Brange.fert <- seq(min(fertilized$biomass), max(fertilized$biomass), .001)
fert.new <- data.frame(biomass = Brange.fert, N=factor("N-fertilized", levels=c("N-fertilized","non-fertilized")))
fert.mods <- model.matrix(~ biomass*N + I(biomass^2)*N, fert.new)[,-1]
fert.pred <- as.data.frame(predict(intact.m, newmods = fert.mods, addx=T, transf=make_pct))

p2 <- ggplot(fert.pred, aes(make_pct(X.biomass), pred)) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_vline(xintercept = 0, lty=2, size=1) + theme_classic() + 
  geom_point(data=fertilized,alpha=0.7,col=carto_pal(12, "Bold")[4], 
             aes(y=make_pct(yi), x=make_pct(biomass), size=1/(fert.m$tau2+vi))) + 
  geom_line (aes(col="Fertilized"),size=0.8) + 
  geom_ribbon(aes(ymax= ci.ub, ymin=ci.lb),fill=carto_pal(12, "Bold")[4], alpha=0.1,size=0.8) +
  geom_line(size=0.8,data=unfert.pred, aes(make_pct(X.biomass), pred, col="Non-fertilized")) +
  geom_ribbon(data=unfert.pred, aes(ymax= ci.ub, ymin=ci.lb),fill=carto_pal(12, "Bold")[3],alpha=0.1) +
  geom_point(data=filtered,alpha=0.7,col=carto_pal(12, "Bold")[3], show.legend = FALSE,
             aes(y=make_pct(yi), x=make_pct(biomass),
                 size=1/(natural.m$tau2+vi))) + 
  geom_line (aes(col="Fertilized"),size=0.8) + 
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  scale_size(range = c(1, 6)) +
  guides(size="none") + 
  scale_colour_manual(name=element_blank(), values = c("Fertilized" = "#F2B701",
                                                       "Non-fertilized" = "#3969AC")) +
  geom_text(aes(50,60, label=(paste(expression("y = 0.1 - 0.17 x + 0.06 x"^2*", p = 0.3453")))),parse = TRUE) +
  theme_cowplot()+
  theme(legend.position = c(.5,.7),
        legend.box = 'horizontal',
        legend.background = element_rect(fill = NA))
p2


## SOC ~ BIOMASS * MYC ##
p12 <- ggplot(unfert.pred, aes(make_pct(X.biomass), pred)) +
  geom_point(data=filtered,
             #col="#7fc97f", show.legend = FALSE,
             aes(y=make_pct(yi), x=make_pct(biomass), col=Myc,
                 size=1/vi)) + 
  scale_color_manual(values=mycols) +
  geom_line (size=0.8) + 
  geom_ribbon(aes(ymax= ci.ub, ymin=ci.lb), alpha=0.1) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  scale_size(range = c(1, 6)) +
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_vline(xintercept = 0, lty=2, size=1) + theme_classic() + 
  guides(size=FALSE,colour=guide_legend(title = NULL,override.aes = list(size = 3))) + 
  scale_x_continuous(breaks = seq(-15, 75, by = 15)) +
  scale_y_continuous(breaks = seq(-20, 40, by = 10), limits = c(-23,40)) +
  theme_cowplot() +
  theme(legend.position= c(0.8,0.9),
        legend.text.align = 0,
        legend.spacing.y = unit(0, "cm"))
save_plot("graphs/regression_myc.png",p12, base_asp = 1.3, type = "cairo-png")

## SOC ~ Myc ##
mod.myc <- rma.mv(yi, vi, mods= ~Myc -1,data=filtered, random = ~ 1 | Site / obs)
mod.myc.n <- filtered%>%  group_by(Myc) %>% summarise(n = n())
mod.highN <- rma.mv(yi, vi, mods= ~Myc,data=fertilized, random = ~ 1 | Site / obs) # Myc not important under high N, p=0.9404
mod.highN <- rma.mv(yi, vi,data=fertilized, random = ~ 1 | Site / obs) # Myc not important under high N, p=0.9404
mod.myc.df <- bind_rows(coef(summary(mod.myc)), coef(summary(mod.highN))) %>% 
  mutate(factor=c("AM","AM-ER","ECM","N-fixer","N-fertilized"),
                                                size=c(mod.myc.n$n,nrow(fertilized)),
                                                group="Soils")
### Biomass ~ Myc ###
mod.biomass.myc <- rma.mv(biomass, vi, mods= ~Myc -1,data=filtered, random = ~ 1 | Site / obs)
mod.biomass.myc.n <- filtered%>%  group_by(Myc) %>% summarise(n = n())
mod.biomass.highN <- rma.mv(biomass, vi,data=fertilized, random = ~ 1 | Site / obs)
mod.biomass.myc.df <- bind_rows(coef(summary(mod.biomass.myc)), coef(summary(mod.biomass.highN)) ) %>% 
  mutate(factor=c("AM","AM-ER","ECM","N-fixer","N-fertilized"),size=c(mod.biomass.myc.n$n, nrow(fertilized)),
                                                                group="Plants")
all <- full_join(mod.myc.df,mod.biomass.myc.df)

### Biomass & SOC ~ Myc  ###
myco <- ggplot(filter(all, factor!="N-fixer", factor!="AM-ER", factor!="N-fertilized"), aes(x=factor, y=make_pct(estimate), color=group, group=group)) +
  geom_hline(yintercept = 0, lty=2, size=1) + 
  scale_color_manual(values=mycols_vegsoil) +
  geom_pointrange(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub)), 
                  position = position_dodge(width = 0),  size=.8) +
  ylab(expression(paste(CO[2]," effect on carbon pools (%)", sep=""))) + xlab("Nutrient-acquisition strategy") +
  #scale_y_continuous(breaks = seq(-20, 40, by = 10), limits = c(-23,40)) +
  theme_cowplot(font_size=8) +
  theme(legend.title = element_blank(),legend.direction = "horizontal",
        legend.position = c(0, 0.99))

#### N UPTAKE boxplot ####
mean_size <- 3
nup <-read.csv("Nuptake.csv") %>% filter(NFERT == "Nlow", myc != "N-fixing")
mod.nup.myc <- rma.mv(yi, vi, mods= ~myc -1,data=nup)
mod.nup.myc.n <- nup%>%  group_by(myc) %>% summarise(n = n())
mod.nup.myc.df <- as.data.frame(coef(summary(mod.nup.myc))) %>% 
  mutate(factor=c("AM","ECM"),
         size=c(mod.nup.myc.n$n))

nup.p <- ggplot(mod.nup.myc.df, aes(factor, make_pct(estimate))) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_pointrange(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub)),size=.8, color=mycols_vegsoil[1]) +
  ylab(expression(paste(CO[2]," effect on N-uptake (%)", sep=""))) + xlab("Nutrient-acquisition strategy") +
  theme_cowplot(font_size=8) +
  theme(legend.position="none",
        axis.title.y = element_text(margin = margin(r=1)))


## MAOM META##
read.csv("eCO2 POM-MAOM - Sheet1.csv") %>% filter(Myc != "Nfixer") %>% 
  summarise(mean(MAOM.amb), mean(POM.amb))
frac <- read.csv("eCO2 POM-MAOM - Sheet1.csv") %>% filter(Myc != "Nfixer") %>%
  mutate(amb.SD= MAOM.amb.se/sqrt(MAOM.amb.n), elev.SD=MAOM.elev.se/sqrt(MAOM.elev.n))
frac$obs <- 1:nrow(frac)
library(metagear)
set.seed(1)
maom <- impute_SD(frac, columnSDnames= c("amb.SD", "elev.SD"), columnXnames=c("MAOM.amb", "MAOM.elev"), 
                range = 10, M = 1)
maom<- escalc(measure="ROM",n1i=MAOM.elev.n,n2i=MAOM.amb.n,m1i=MAOM.elev,m2i=MAOM.amb,
              sd1i=elev.SD,sd2i=amb.SD, data=maom)
mod.maom.myc <- rma.mv(yi, vi, mods= ~Myc -1, data=maom, slab=paste(Site, Experiment, sep=", "))
mod.maom.myc.n <- maom%>%  group_by(Myc) %>% summarise(n = n())
mod.maom.myc.df <- as.data.frame(coef(summary(mod.maom.myc))) %>% 
  mutate(factor=c("AM","ECM"),
         size=c(mod.maom.myc.n$n))
maom.p <- ggplot(mod.maom.myc.df,aes(factor, make_pct(estimate))) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_pointrange(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub)),size=.8, color=mycols_vegsoil[2]) +
  ylab(expression(paste(CO[2]," effect on MAOM (%)", sep=""))) + xlab("Nutrient-acquisition strategy") +
  theme_cowplot(font_size=8) +
  theme(legend.position="none",
        axis.title.y = element_text(margin = margin(r=1)))

## POM META##
frac2 <- read.csv("eCO2 POM-MAOM - Sheet1.csv") %>% filter(Myc != "Nfixer") %>%
  mutate(amb.SD= POM.amb.se/sqrt(POM.amb.n), elev.SD=POM.elev.se/sqrt(POM.elev.n))
frac2$obs <- 1:nrow(frac2)
library(metagear)
set.seed(1)
pom <- impute_SD(frac2, columnSDnames= c("amb.SD", "elev.SD"), columnXnames=c("POM.amb", "POM.elev"), range = 10, M = 1)
pom<- escalc(measure="ROM",n1i=POM.elev.n,n2i=POM.amb.n,m1i=POM.elev,m2i=POM.amb,
              sd1i=elev.SD,sd2i=amb.SD, data=pom)
mod.pom.myc <- rma.mv(yi, vi, mods= ~Myc -1, data=pom, slab=paste(Site, Experiment, sep=", "))
mod.pom.myc.n <- pom%>%  group_by(Myc) %>% summarise(n = n())
mod.pom.myc.df <- as.data.frame(coef(summary(mod.pom.myc))) %>% 
  mutate(factor=c("AM","ECM"),size=c(mod.pom.myc.n$n))
pom.p <- ggplot(mod.pom.myc.df,aes(factor, make_pct(estimate))) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_pointrange(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub)),size=.8, color=mycols_vegsoil[2]) +
  ylab(expression(paste(CO[2]," effect on POM (%)", sep=""))) + xlab("Nutrient-acquisition strategy") +
  theme_cowplot(font_size=8) +
  theme(legend.position="none",
        axis.title.y = element_text(margin = margin(r=1)))
save_plot("pom.png",pom.p,base_width=2, base_height=2,type = "cairo-png", bg="white")

##### SUPER FIG. 1 #####
right2<- plot_grid(nup.p + xlab("") + theme(plot.margin = unit(c(5,5,-10,5), "points")),
                   maom.p + xlab("") + theme(plot.margin = unit(c(0,5,5,5), "points")),
                   nrow=2, labels = c("c","d"), align="v", axis="l",
                   vjust = 1.2, hjust = 0.5, label_size=10)
middleright <- plot_grid(myco + xlab("") + theme(plot.margin = unit(c(5,5,5,0), "points")),
                         right2,
                         vjust = 1.2,
                         axis = "b",
                         labels = c("b",""), label_size=10,
                         rel_widths = c(1, .7),
                         nrow = 1, ncol=2)
super <- plot_grid(p1 +theme_cowplot(font_size=8) + theme(plot.margin = unit(c(5,5,5,5), "points")), 
                   middleright,
                   vjust = 1.2, axis = "b", labels = c("a",""), 
                   label_size=10,
                   rel_widths = c(1, .7))
save_plot("graphs/superFig1.png", super, ncol=2, nrow=1, base_height = 3, base_width = 3, type = "cairo-png",dpi= 1600)
save_plot("graphs/superFig1.pdf", super, ncol=2, nrow=1, base_height = 3, base_width = 3, device = cairo_pdf, fallback_resolution = 1200)

##### FERTILIZED #####
nhigh <- ggplot(filter(all, factor=="N-fertilized"), aes(x=factor, y=make_pct(estimate), color=group, group=group)) +
  geom_hline(yintercept = 0, lty=2, size=1) + 
  scale_color_manual(values=mycols_vegsoil) +
  geom_pointrange(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub)), 
                  position = position_dodge(width = 0),  size=1) +
  ylab(expression(paste(CO[2]," effect on C pools (%)", sep=""))) + xlab("") +
  #scale_y_continuous(breaks = seq(-20, 40, by = 10), limits = c(-23,40)) +
  theme_cowplot() +
  theme(legend.title = element_blank(),legend.direction = "horizontal",
        legend.position = c(0, 1))
plot_Nhigh <- plot_grid(p2 + theme(plot.margin = unit(c(10,5,5,5), "points")), 
                        nhigh + theme(plot.margin = unit(c(10,10,5,5), "points")),
                        align="hv",
                        labels = "auto",
                        hjust = -.1,
                        vjust = 1.2,
                        rel_widths = c(1, 0.5),
                        nrow = 1, ncol=2)

save_plot("graphs/regression_fertilized.png",plot_Nhigh, ncol=2, nrow=1, dpi= 800, base_width=3.5, type = "cairo-png", bg="white")

#### Root biomass boxplot ####
root <-read.csv("root_biomass.csv") %>% filter(newN == "Nlow", System.Type != "Scrubland") %>%
  mutate(System.Type=recode_factor(System.Type, Grassland="Grassland",'Tree Stand'="Forest"))
root %>% group_by(Myc) %>%
  summarise(mean=make_pct(mean(es)), SE=make_pct(sd(es)/sqrt(n())), n=n())
root %>% group_by(System.Type) %>%
  summarise(mean=make_pct(mean(es)), SE=make_pct(sd(es)/sqrt(n())), n=n())
root.Myc <- ggplot(root, aes(Myc, make_pct(es))) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_boxplot(alpha=0.7, fill=mycols_vegsoil[1]) +
  ylab(expression(paste(CO[2]," effect on root biomass (%)", sep=""))) + xlab("Nutrient-acquisition strategy") +
  theme_cowplot() +
  theme(legend.position="none")
root.Veg <- ggplot(root, aes(System.Type, make_pct(es))) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_boxplot(alpha=0.7, fill=mycols_vegsoil[1]) +
  ylab(expression(paste(CO[2]," effect on root biomass (%)", sep=""))) + xlab("Ecosystem type") +
  theme_cowplot() +
  theme(legend.position="none")

#### Fine-root boxplot ####
fr <-read.csv("FR.csv") %>% filter(NFERT == "Nlow", myc != "N-fixing") %>%
  mutate(ecosystem.type=recode_factor(ecosystem.type, grassland="Grassland",forest = "Forest"))
fr %>% group_by(myc) %>%
  summarise(mean=make_pct(mean(FR)), SE=make_pct(sd(FR)/sqrt(n())), n=n())
fr %>% group_by(ecosystem.type) %>%
  summarise(mean=make_pct(mean(FR)), SE=make_pct(sd(FR)/sqrt(n())), n=n())

fr.boxplot <- ggplot(fr, aes(myc, make_pct(FR))) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_boxplot(alpha=0.7, fill=carto_pal(12, "Bold")[3]) +
  ylab(expression(paste(CO[2]," effect on fine-root production (%)", sep=""))) + xlab("Nutrient-acquisition strategy") +
  theme_cowplot() +
  theme(legend.position="none")

fr.Veg <- ggplot(fr, aes(ecosystem.type, make_pct(FR))) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_boxplot(alpha=0.7, fill=carto_pal(12, "Bold")[3]) +
  ylab(expression(paste(CO[2]," effect on fine-root production (%)", sep=""))) + xlab("Ecosystem type") +
  theme_cowplot() +
  theme(legend.position="none")


######## SOC ########
soc.Myc <- ggplot(filter(filtered, Myc=="AM" | Myc=="ECM"), aes(Myc, amb)) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_boxplot(alpha=0.7, fill=mycols_vegsoil[2]) +
  ylab(expression(paste("Soil C stocks  (g ", m^-2,")",sep=""))) + xlab("Nutrient-acquisition strategy") +
  theme_cowplot() +
  theme(legend.position="none")
soc.Veg <- ggplot(filter(filtered, Ecosystem.type=="Grassland" | Ecosystem.type=="Tree Stand") %>% 
                    mutate(Ecosystem.type=recode(Ecosystem.type, 'Tree Stand'="Forest")), aes(Ecosystem.type, amb)) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_boxplot(alpha=0.7, fill=mycols_vegsoil[2]) +
  ylab(expression(paste("Soil C stocks  (g ", m^-2,")",sep=""))) + xlab("Ecosystem type") +
  theme_cowplot() +
  theme(legend.position="none")

######### LITTER CN #######
read.csv("DB_litter_CNP.csv") %>% filter(response=="leaf_litter_cn", is.na(nutrient_av) | nutrient_av=="poor") %>%
  mutate(id=paste0(exp, dominant_species)) %>% group_by(id) %>% summarise(n())
read.csv("DB_litter_CNP.csv") %>% filter(response=="leaf_litter_cn", is.na(nutrient_av) | nutrient_av=="poor") %>%
  mutate(yi=log(x_t/x_c)) %>% summarise(make_pct(mean(yi)))
read.csv("DB_litter_CNP.csv") %>% filter(response=="leaf_litter_cn", is.na(nutrient_av) | nutrient_av=="poor") %>%
  mutate(yi=log(x_t/x_c),Myc=recode_factor(myco,AM="AM", EcM="ECM", both="ECM")) %>% group_by(Myc) %>% summarise(mean(x_c))
lit.Myc<-read.csv("DB_litter_CNP.csv") %>% filter(response=="leaf_litter_cn", is.na(nutrient_av) | nutrient_av=="poor") %>%
  mutate(yi=log(x_t/x_c), Myc=recode_factor(myco,AM="AM", EcM="ECM", both="ECM")) %>%
  ggplot(aes(Myc,make_pct(yi))) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_boxplot(alpha=0.7, fill=carto_pal(12, "Bold")[1]) +
  ylab(expression(paste(CO[2]," effect on litter C:N (%)", sep=""))) + xlab("Nutrient-acquisition strategy") +
  theme_cowplot() +
  theme(legend.position="none")

lit.Veg<-read.csv("DB_litter_CNP.csv") %>% filter(response=="leaf_litter_cn", is.na(nutrient_av) | nutrient_av=="poor") %>%
  mutate(yi=log(x_t/x_c), Ecosystem.type=recode_factor(ecosystem, grassland="Grassland",shrubland="Grassland", tree_stand="Forest")) %>%
  ggplot(aes(Ecosystem.type,make_pct(yi))) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_boxplot(alpha=0.7, fill=carto_pal(12, "Bold")[1]) +
  ylab(expression(paste(CO[2]," effect on litter C:N (%)", sep=""))) + xlab("Ecosystem type") +
  theme_cowplot() +
  theme(legend.position="none")

### ROOT - SOC MULTI ###
rootsoc.multi <- plot_grid(root.Veg + theme(plot.margin = unit(c(10,5,5,20), "points")), root.Myc + theme(plot.margin = unit(c(10,5,5,20), "points")), 
                           fr.Veg, fr.boxplot,
                           lit.Veg, lit.Myc,
                           soc.Veg, soc.Myc,
                           align="hv",
                           labels = "auto",
                           #hjust = 0,
                           vjust = 1.2,
                           nrow = 4, ncol=2)

save_plot("graphs/RootsSOC.png",rootsoc.multi, ncol=2, nrow=4, dpi= 800, base_width=5, type = "cairo-png", bg="white")
