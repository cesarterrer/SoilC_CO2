source("Effect Size.R")
library(ggplot2)
library(cowplot)
library(ggrepel)
library(rcartocolor)
options(bitmapType="cairo")
# display_carto_all()
display_carto_pal(12, "Bold")
mycols <- carto_pal(12, "Bold")[c(4,2,1,7)]
mycols2 <- carto_pal(12, "Bold")[c(7,8)]
mycols_vegsoil <- carto_pal(12, "Bold")[c(2,4)]
make_pct <- function(x) (exp(x) - 1) * 100

#dat <- read.csv("soilC_meta.csv")
dat <- filter(dat, biomass != "NA") # Remove experiments with missing biomass data
dat <- filter(dat, nyears >= 0.5) # Remove experiments of less than 6 months duration


filtered <- filter(dat, N=="Nlow", Experiment_type != "Chamber", Disturbance=="intact")
filtered$obs <- 1:nrow(filtered)
fertilized <- filter(dat, N=="Nhigh")
## BIOMASS ##
summary(rma(yi, vi, mods= ~biomass * Myc + amb + Depth..cm., data=filtered, knha=TRUE, control=list(stepadj=0.5)))

#mod <- rma.mv(yi, vi, mods= ~biomass + amb + Depth..cm.,data=filtered, random = ~ 1 | Site / obs)
mod <- rma.mv(yi, vi, mods= ~biomass,data=filtered, random = ~ 1 | Site / obs)
summary(mod) 

Brange <- seq(min(filtered$biomass), max(filtered$biomass), .001)
Biomass.new <- data.frame(biomass = Brange, 
                          #Myc=factor("AM", levels=c("AM","ECM","ER", "Nfixer")),
                          amb = mean(filtered$amb),
                          Depth..cm.=15)
Biomass_mods <- model.matrix(~ biomass + amb + Depth..cm., Biomass.new)[,-1]
preds <- as.data.frame(predict(mod, newmods = Biomass_mods, addx=T, transf=make_pct))
preds <- as.data.frame(predict(mod, newmods = Brange,addx=T, transf=make_pct))

p1 <- ggplot(preds, aes(make_pct(X.biomass), pred)) + 
  geom_point(data=filtered,
             col=carto_pal(12, "Bold")[3], show.legend = FALSE,
             aes(y=make_pct(yi), x=make_pct(biomass), col=Myc,
                 size=1/vi)) + 
  geom_line (size=0.8) + 
  geom_ribbon(aes(ymax= ci.ub, ymin=ci.lb), alpha=0.1) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  scale_size(range = c(1, 6)) +
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_vline(xintercept = 0, lty=2, size=1) + theme_classic() + 
  guides(size=FALSE,col = guide_legend(title = NULL)) + 
  scale_x_continuous(breaks = seq(-15, 75, by = 15)) +
  scale_y_continuous(breaks = seq(-20, 40, by = 10), limits = c(-23,40)) +
  #geom_text_repel(data=limited, aes(y=make_pct(yi), x=make_pct(biomass),label=Experiment)) +
  #theme_minimal() + theme(panel.grid.major.x = element_blank())
  theme_cowplot()
p1
save_plot("graphs/regression.png",p1, type = "cairo-png",base_aspect_ratio = 1.3)

## BIOMASS * MYC ##
p12 <- ggplot(preds, aes(make_pct(X.biomass), pred)) + 
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
  guides(size=FALSE,col = guide_legend(title = NULL)) + 
  scale_x_continuous(breaks = seq(-15, 75, by = 15)) +
  scale_y_continuous(breaks = seq(-20, 40, by = 10), limits = c(-23,40)) +
  theme_cowplot() +
  theme(legend.position= c(0.8,0.9),
        legend.text.align = 0,
        legend.spacing.y = unit(0, "cm"))
p12
save_plot("graphs/regression2.png",p12, type = "cairo-png",base_aspect_ratio = 1.3)

## SOC ~ MYC ##
mod.myc <- rma(yi, vi, mods= ~Myc -1,data=filtered, control=list(stepadj=0.5), knha=TRUE)
mod.myc.n <- filtered%>%  group_by(Myc) %>% summarise(n = n())
mod.myc.df <- coef(summary(mod.myc)) %>% mutate(factor=c("AM","ECM","ER","Nfixer"),
                                                size=mod.myc.n$n,
                                                group="Soils")

p2 <- ggplot(mod.myc.df, aes(x=factor, y=make_pct(estimate))) +
  #geom_crossbar(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub), fill=factor), fill = "grey80",alpha = 0.6, width = 0.5) +
  geom_jitter(data=filtered, aes(x=Myc, y= make_pct(yi), size = 1/vi, col=Myc), position = position_jitter(w = 0.2, h = 0), alpha = 0.8) +
  scale_color_manual(values=mycols) +
  scale_size(range = c(1, 6)) +
  #geom_point(color = 'black', pch=21,size=5, fill="black")+
  #geom_errorbar(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub)), width=.1)+
  geom_pointrange(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub)), size=1) +
  guides(size=FALSE, col=FALSE) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  ylab(expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) + xlab(NULL)+
  scale_y_continuous(breaks = seq(-20, 40, by = 10), limits = c(-20,40)) +
  #scale_y_continuous(breaks = seq(-20, 80, by = 20), limits = c(-20,80)) +
  #theme_minimal() + theme(panel.grid.major.x = element_blank())
  theme_cowplot()

### Regression + MYC ###
options(bitmapType="cairo")
prow <- plot_grid(p1, p2+ ylab(""),
                  align = 'hv',
                  labels = c("a", "b"),
                  hjust = -2,
                  nrow = 1, ncol=2)
save_plot("graphs/Fig2.pdf", prow, ncol=2, nrow=1, base_width=3.5, device = cairo_pdf, fallback_resolution = 1200)
save_plot("graphs/Fig2.png", prow, ncol=2, nrow=1, dpi= 800, base_width=3.5,type = "cairo-png")

### Biomass ~ Myc ###
mod.biomass.myc <- rma(biomass, vi, mods= ~Myc -1,data=filtered, control=list(stepadj=0.5), knha=TRUE)
mod.biomass.myc.n <- filtered%>%  group_by(Myc) %>% summarise(n = n())
mod.biomass.myc.df <- coef(summary(mod.biomass.myc)) %>% mutate(factor=c("AM","ECM","ER","Nfixer"),
                                                                size=mod.biomass.myc.n$n,
                                                                group="Plants")
all <- full_join(mod.myc.df,mod.biomass.myc.df)

p2b <- ggplot(mod.biomass.myc.df, aes(x=factor, y=make_pct(estimate))) +
  geom_jitter(data=filtered, aes(x=Myc, y= make_pct(biomass), size = 1/vi, col=Myc), position = position_jitter(w = 0.2, h = 0), alpha = 0.8) +
  scale_color_manual(values=mycols) +
  scale_size(range = c(1, 6)) +
  #geom_point(color = 'black', pch=21,size=5, fill="black")+
  #geom_errorbar(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub)), width=.1)+
  geom_pointrange(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub)), size=1) +
  guides(size=FALSE, col=FALSE) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  ylab(expression(paste(CO[2]," effect on plant biomass (%)", sep=""))) + xlab(NULL)+
  scale_y_continuous(breaks = seq(-15, 75, by = 15)) +
  #scale_y_continuous(breaks = seq(-20, 80, by = 20), limits = c(-20,80)) +
  #theme_minimal() + theme(panel.grid.major.x = element_blank())
  theme_cowplot()

### biomas&soil ~ Myc 2-plot ###
meta <- plot_grid(p2b + xlab(NULL), p2,
                  align = 'hv',
                  labels = c("b", "c"),
                  hjust = -2,
                  nrow = 2, ncol=1)


### biomas&soil ~ Myc 1-plot ###
myco <- ggplot(filter(all, factor!="Nfixer", factor!="ER"), aes(x=factor, y=make_pct(estimate), color=group, group=group)) +
  scale_color_manual(values=mycols_vegsoil) +
  geom_pointrange(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub)), 
                  position = position_dodge(width = 0),  size=1) +
  geom_hline(yintercept = 0, lty=2, size=1) + 
  ylab(expression(paste(CO[2]," effect on C pools (%)", sep=""))) + xlab("Nutrient-acquisition strategy") +
  scale_y_continuous(breaks = seq(-20, 40, by = 10), limits = c(-23,40)) +
  theme_cowplot() +
  theme(legend.title = element_blank(),legend.direction = "horizontal",
        legend.position = c(0, 0.95))
myco
save_plot("graphs/myco.png",myco, type = "cairo-png",base_aspect_ratio = 1.3)

#### REGRESSION + biomas&soil ~ Myc #####
plot_final <- plot_grid(p1, myco,
                        align="hv",
                        labels = "auto",
                        hjust = -2,
                        nrow = 1, ncol=2)
plot_final
save_plot("graphs/Fig2v2.pdf", plot_final, ncol=2, nrow=1, base_width=3.5, device = cairo_pdf, fallback_resolution = 1200)
save_plot("graphs/Fig2v2.png", plot_final, ncol=2, nrow=1, dpi= 800, base_width=3.5, type = "cairo-png")

#### N UPTAKE boxplot ####
nup <-read.csv("Nup.csv") %>% filter(NFERT == "Nlow", myc != "N-fixing")

nup.p <- ggplot(nup, aes(myc, make_pct(Nupr), fill=myc)) + 
  geom_boxplot(alpha=0.7) +
  #stat_summary(fun=mean, geom="point", shape=23, size=4) +
  #geom_violin() +
  #geom_point(aes(fill=myc),size=3,shape = 21, position = position_jitter(width = 0.01)) +
  geom_dotplot(aes(fill=myc),binaxis='y', stackdir='center', dotsize=1, binwidth = 2) +
  scale_color_manual(values=mycols2) +
  scale_fill_manual(values=mycols2) +
  geom_hline(yintercept = 0, lty=2, size=1) + 
  ylab(expression(paste(CO[2]," effect on N-uptake (%)", sep=""))) + xlab("Nutrient-acquisition strategy") +
  scale_y_continuous(breaks = seq(-20, 40, by = 10), limits = c(-23,40)) +
  theme_cowplot() +
  theme(legend.position="none")

