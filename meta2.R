library(devtools) 
install_github("wviechtb/metafor") 
library(metafor)
library(dplyr) 
library(ggplot2)
library(metagear)
library(cowplot)
library(ggrepel)
make_pct <- function(x) (exp(x) - 1) * 100

dat <- read.csv("soilC_meta.csv")
dat <- filter(dat, biomass != "NA")
dat <- filter(dat, nyears >= 0.5)

# Nitrogen
Nm <- rma(yi, vi, data=dat, mods=~N -1, knha=TRUE)
Nm
Nm.n <- dat %>%  group_by(N) %>% summarise(n = n())
Nm.df <- coef(summary(Nm)) %>% mutate(type="Nitrogen fertilization", 
                                          factor=as.factor(c("Yes", "No")),
                                          size=Nm.n$n)


# Ecosystem.type
Ecom <- rma(yi, vi, data=dat, mods=~Ecosystem.type -1, knha=TRUE, subset= Ecosystem.type !="Wetland")
Ecom
Ecom.n <- dat %>%  group_by(Ecosystem.type) %>% summarise(n = n())
Ecom.df <- coef(summary(Ecom)) %>% mutate(type="Ecosystem type", 
                                          factor=levels(dat$Ecosystem.type)[1:4],
                                          size=Ecom.n$n[1:4])
# Experiment.type
Expm <- rma(yi, vi, data=dat, mods=~Experiment_type -1, knha=TRUE)
Expm
Expm.n <- dat %>%  group_by(Experiment_type) %>% summarise(n = n())
Expm.df <- coef(summary(Expm)) %>% mutate(type="Experiment type", 
                                              factor=levels(dat$Experiment_type),
                                              size=Expm.n$n)
# Biome
Biomem <- rma(yi, vi, data=dat, mods=~Biome -1, knha=TRUE)
Biomem
Biomem.n <- dat %>%  group_by(Biome) %>% summarise(n = n())
Biomem.df <- coef(summary(Biomem)) %>% mutate(type="Biome", 
                                                          factor=levels(dat$Biome),
                                                          size=Biomem.n$n)
# Disturbance
Disturbancem <- rma(yi, vi, data=dat, mods=~Disturbance -1, knha=TRUE)
Disturbancem
Disturbancem.n <- dat %>%  group_by(Disturbance) %>% summarise(n = n())
Disturbancem.df <- coef(summary(Disturbancem)) %>% mutate(type="Disturbance", 
                                          factor=c("Disturbed","Intact"),
                                          size=Disturbancem.n$n)

# Myc
Mycm <- rma(yi, vi, data=dat, mods=~Myc -1, knha=TRUE, subset= Myc != "NM")
Mycm.n <- dat %>%  group_by(Myc) %>% summarise(n = n())
Mycm.df <- coef(summary(Mycm)) %>% mutate(type="Symbiosis", 
                                          factor=c("AM","EcM","N-fixer"),
                                          size=Mycm.n$n[1:3])
#### META PLOT ####
meta.df <- bind_rows(Ecom.df, Nm.df, Expm.df, Disturbancem.df, Mycm.df)

pdf("graphs/figure1.pdf",height=5, width=3, useDingbats=FALSE)
par(mar=c(4,4,1,2))
forest(x=meta.df$estimate,sei=meta.df$se,slab=meta.df$factor, annotate=FALSE, xlim=c(-30, 10),
       ilab=paste0("(",meta.df$size,")"),ilab.xpos=-11,
       psize=1,transf=make_pct, at=c(-10, 0, 10, 20), xlab="CO2 effect on soil C (%)",
       subset=14:1, rows=c(1:3,6:7,10:12,15:16,19:22),ylim=c(-1, 27),cex=0.75)
op <- par(cex=.75, font=4)
text(-30, c(4,8,13,17,23), pos=4, c("Symbiosis","Disturbance","Experiment type", "Nitrogen fertilization", "Ecosystem type"))
par(cex=0.75, font=1)
dev.off()

#My APA-format theme
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        legend.position='none')

p <- ggplot(meta.df, aes(y=factor, x=make_pct(estimate), 
      xmax=make_pct(ci.ub),xmin=make_pct(ci.lb), 
      label=paste("(n=",size,")",sep=""), shape = type))+
  geom_point(color = 'black', pch=21,size=5, fill="black")+
  geom_errorbarh(height=.1)+ ylab(NULL) +
  scale_x_continuous(limits=c(-10,30), name="CO2 effect on soil C (%)")+
  geom_vline(xintercept=0, color='black', lty=2, size=1)+
  facet_grid(type~., scales= 'free', space='free')+
  apatheme
p + theme(strip.text = element_text(size = 12, face = "bold"),
          strip.background = element_blank(),
          panel.spacing.y=unit(1,"cm"),
          axis.text.y = element_text(size=12))
ggsave(p, file='ggforest.png', width = 8, height=8, dpi=300)

########################### MODEL SELECTION #####################

### GLMULTI ###
library(glmulti)
rma.glmulti <- function(formula, data, ...)
  rma(formula, vi, data=data, method="ML", ...)
# No interactions
res <- glmulti(yi ~ Myc + N + biomass + nyears + Experiment_type + Disturbance + Ecosystem.type, data=dat,
               #method="d",
               level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=128)
plot(res, type="s") # Disturbance and Myc
limited <- filter(dat,  N=="Nlow")
intact_limited <- filter(dat, Disturbance == "intact", N=="Nlow")
my_rma.glmulti <- function(formula, data, ...) {
  rma(formula, vi, data=data, method="ML", control=list(stepadj=0.5), ...)
}
res_intact_limited <- glmulti(yi ~ Myc + biomass + nyears + Experiment_type + Ecosystem.type, 
                              data=intact_limited,
               level=1, fitfunction=my_rma.glmulti, crit="aicc", confsetsize=128)
plot(res_intact_limited, type="s")
eval(metafor:::.glmulti)
coef(res)
mmi <- as.data.frame(coef(res))
mmi <- data.frame(Estimate=mmi$Est, SE=sqrt(mmi$Uncond), Importance=mmi$Importance, row.names=row.names(mmi))
mmi$z <- mmi$Estimate / mmi$SE
mmi$p <- 2*pnorm(abs(mmi$z), lower.tail=FALSE)
names(mmi) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
mmi$ci.lb <- mmi[[1]] - qnorm(.975) * mmi[[2]]
mmi$ci.ub <- mmi[[1]] + qnorm(.975) * mmi[[2]]
mmi <- mmi[order(mmi$Importance, decreasing=TRUE), c(1,2,4:7,3)]
modsel <- round(mmi, 4)
write.csv(modsel,"model_selection.csv")

### MuMIn ###
library(MuMIn)
eval(metafor:::.MuMIn)
full <- rma(yi, vi, data=dat, mods = ~ Myc + N + nyears + biomass + Ecosystem.type + Experiment_type + Disturbance +
              N:Myc + Experiment_type:Myc + Experiment_type:N + biomass:N,
            method="ML", knha=TRUE)
res2 <- dredge(full, trace=2)
subset(res2, delta <= 2, recalc.weights=FALSE)
summary(model.avg(res2, revised.var=FALSE))
MuMIn::importance(res2)
plot(res2)

  #### META-FOREST ####
install_github("cjvanlissa/metaforest")
library(metaforest)
forest <- MetaForest(yi~ Myc + N + biomass + nyears + Ecosystem.type + Experiment_type + Disturbance,
                         data = dat,
                         whichweights = "random",
                         num.trees = 20000)
summary(forest)
VarImpPlot(forest)

################# FILTERING BASED ON INTERACTIONS ###############
filtered <- filter(dat, N=="Nlow", Experiment_type != "Chamber", Disturbance=="intact")

## METAFOR ##
summary(rma(yi, vi, mods= ~biomass + Myc, data=filtered, knha=TRUE, control=list(stepadj=0.5)))

mod <- rma(yi, vi, mods= ~biomass,data=filtered, knha=TRUE, control=list(stepadj=0.5))
summary(mod) 
preds <- as.data.frame(predict(mod, newmods = c(seq(min(filtered$biomass), max(filtered$biomass), .001)),addx=T, transf=make_pct))

p1 <- ggplot(preds, aes(make_pct(X.biomass), pred)) + 
  geom_point(data=filtered,
             col="#7fc97f", show.legend = FALSE,
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

mod.myc <- rma(yi, vi, mods= ~Myc -1,data=filtered, control=list(stepadj=0.5), knha=TRUE)
mod.myc.n <- filtered%>%  group_by(Myc) %>% summarise(n = n())
mod.myc.df <- coef(summary(mod.myc)) %>% mutate(factor=c("AM","ECM","Nfixer"),
                                          size=mod.myc.n$n)
library(rcartocolor)
# display_carto_all()
display_carto_pal(12, "Bold")
mycols <- carto_pal(12, "Bold")[c(4,3,1)]

p2 <- ggplot(mod.myc.df, aes(x=factor, y=make_pct(estimate))) +
#geom_crossbar(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub), fill=factor), fill = "grey80",alpha = 0.6, width = 0.5) +
geom_jitter(data=filtered, aes(x=Myc, y= make_pct(yi), size = 1/vi, col=Myc), position = position_jitter(w = 0.2, h = 0), alpha = 0.8) +
  scale_color_manual(values=mycols) +
  scale_size(range = c(1, 6)) +
  geom_point(color = 'black', pch=21,size=5, fill="black")+
  geom_errorbar(aes(ymin=make_pct(ci.lb), ymax=make_pct(ci.ub)), width=.1)+
  guides(size=FALSE, col=FALSE) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  ylab(expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) + xlab(NULL)+
  scale_y_continuous(breaks = seq(-30, 40, by = 10), limits = c(-23,40)) +
  #theme_minimal() + theme(panel.grid.major.x = element_blank())
  theme_cowplot()

options(bitmapType="cairo")
prow <- plot_grid(p1, p2+ ylab(""),
                   align = 'hv',
                   labels = c("a", "b"),
                   hjust = -2,
                   nrow = 1, ncol=2)
save_plot("graphs/Fig2.pdf", prow, ncol=2, nrow=1, base_width=3.5, device = cairo_pdf, fallback_resolution = 1200)
save_plot("graphs/Fig2.png", prow, ncol=2, nrow=1, dpi= 800, base_width=3.5,type = "cairo-png")
