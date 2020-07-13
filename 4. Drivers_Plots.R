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

#dat <- read.csv("soilC_meta.csv")
dat <- filter(dat, biomass != "NA") # Remove experiments with missing biomass data
dat <- filter(dat, nyears >= 0.5) # Remove experiments of less than 6 months duration
dat$obs <- 1:nrow(dat)
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
  geom_point(data=fertilized,alpha=0.7,
             col=carto_pal(12, "Bold")[3], show.legend = FALSE,
             aes(y=make_pct(yi), x=make_pct(biomass), col=Myc,
                 size=1/(fert.m$tau2+vi))) + 
  #geom_line (size=0.8) + 
  #geom_ribbon(aes(ymax= ci.ub, ymin=ci.lb), alpha=0.1) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  scale_size(range = c(1, 6)) +
  guides(size=FALSE,col = guide_legend(title = NULL)) + 
  #scale_x_continuous(breaks = seq(-15, 75, by = 15), limits = c(-15,75)) +
  #scale_y_continuous(breaks = seq(-20, 40, by = 10), limits = c(-23,40)) +
  #geom_text_repel(data=filtered, aes(y=make_pct(yi), x=make_pct(biomass),label=paste(Experiment,Myc))) +
  theme_cowplot()
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
  ylab(expression(paste(CO[2]," effect on C pools (%)", sep=""))) + xlab("Nutrient-acquisition strategy") +
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

#### Fine-root boxplot ####
fr <-read.csv("FR.csv") %>% filter(NFERT == "Nlow", myc != "N-fixing")

fr.boxplot <- ggplot(fr, aes(myc, make_pct(FR))) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_boxplot(alpha=0.7, fill=mycols_vegsoil[1]) +
  #stat_summary(fun = "mean", geom = "point",  size=mean_size,shape=18, colour="red") +
  #stat_summary(fun.data = mean_se, geom = "errorbar",width=.1, colour="red") +
  ylab(expression(paste(CO[2]," effect on fine-root production (%)", sep=""))) + xlab("Nutrient-acquisition strategy") +
  theme_cowplot(font_size=8) +
  theme(legend.position="none")
ggsave("graphs/fineroot_prod.png", width = 2.5, height = 2.5)

#### Fine-root vs Biomass ####
fr <-read.csv("FR.csv")
fr.bio <- left_join(fr,dat) %>% filter(N=="non-fertilized", Experiment_type != "Chamber", Disturbance=="intact")
ggplot(fr.bio,aes(biomass,FR)) + geom_point()

#### MAOM - POM ####
frac <- read.csv("eCO2 POM-MAOM - Sheet1.csv") %>% filter(Myc != "Nfixer") %>%
  mutate(amb.SD= MAOM.amb.se/sqrt(MAOM.amb.n), elev.SD=MAOM.elev.se/sqrt(MAOM.elev.n))
frac$obs <- 1:nrow(frac)
frac.long <- mutate(frac, POM=log(POM.elev/POM.amb), MAOM=log(MAOM.elev/MAOM.amb)) %>% 
  tidyr::pivot_longer(cols=POM:MAOM)
symmetrise_scale <- function(p, axis = "x"){
  gb <- ggplot_build(p)
  type <- switch(axis, "x" = "x.range", "y" = "y.range")
  
  fname <- setdiff(names(gb$layout$layout), c("PANEL", "ROW", "COL",  "SCALE_X", "SCALE_Y"))  
  facets <- gb$layout$layout[ ,fname, drop=FALSE]
  
  lims <- do.call(cbind, lapply(gb$layout$panel_params, "[[", type))
  lims2 <- as.vector(t(tcrossprod(apply(abs(lims), 2, max), c(-1,1))))
  
  dummy <- setNames(data.frame(facets[rep(seq_len(nrow(facets)), each=2),], lims2), c(fname, axis))
  
  switch(axis, 
         "x" = p + geom_blank(data=dummy, aes(x=x, y=Inf), inherit.aes = FALSE), 
         "y" = p + geom_blank(data=dummy, aes(x=Inf, y=y), inherit.aes = FALSE))
}

frac.p <- ggplot(frac.long, aes(Myc, make_pct(value))) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_boxplot(fill=mycols_vegsoil[2], alpha=0.7,position=position_dodge(0.8)) +
  stat_summary(aes(group=name),fun = "mean", geom = "point",  size=mean_size, position=position_dodge(0.8),show.legend = F,shape=18, colour="red") +
  stat_summary(aes(group=name),fun.data = mean_se, geom = "errorbar",width=.1, position=position_dodge(0.8),show.legend = F, colour="red") +
  ylab(expression(paste(CO[2]," effect on soil fractions (%)", sep=""))) + xlab("Nutrient-acquisition strategy") +
  facet_wrap( ~ name, scales="free_y") +
  theme_cowplot(font_size=10) +
  theme(axis.title.y = element_text(margin = margin(r=1)),
        strip.background = element_rect(colour="black", fill="transparent",size=.5),
        panel.spacing.x = unit(0,"line"), 
        panel.border=element_rect(colour="black",size=.5))
frac.p <- symmetrise_scale(frac2.p, "y")
## MAOM META##
frac <- read.csv("eCO2 POM-MAOM - Sheet1.csv") %>% filter(Myc != "Nfixer") %>%
  mutate(amb.SD= MAOM.amb.se/sqrt(MAOM.amb.n), elev.SD=MAOM.elev.se/sqrt(MAOM.elev.n))
frac$obs <- 1:nrow(frac)
library(metagear)
set.seed(1)
maom <- impute_SD(frac, columnSDnames= c("amb.SD", "elev.SD"), columnXnames=c("MAOM.amb", "MAOM.elev"), 
                range = 10, M = 1)
maom<- escalc(measure="ROM",n1i=MAOM.elev.n,n2i=MAOM.amb.n,m1i=MAOM.elev,m2i=MAOM.amb,
              sd1i=elev.SD,sd2i=amb.SD, data=maom)
mod.maom.myc <- rma.mv(yi, vi, mods= ~Myc -1,
                       #random = ~ 1 | Site / obs
                       data=maom, slab=paste(Site, Experiment, sep=", "))
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

save_plot("graphs/regression_fertilized.png",plot_Nhigh, ncol=2, nrow=1, dpi= 800, base_width=3.5, type = "cairo-png")
