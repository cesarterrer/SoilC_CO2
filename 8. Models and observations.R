library(ggplot2)
library(tidyr)
library(cowplot)
library(ggpmisc)
library(dplyr)
require(mgcv)
require(metafor)
make_pct <- function(x) (exp(x) - 1) * 100

data <- read.csv("model_data_mean.csv")
summary(lm(Csoil~Cplant, data=data))
summary(lm(soil~biomass, data=data))
models <- read.csv("models.csv")
model_data <- left_join(dplyr::select(models, -Cplant),dplyr::select(data,Site,Cplant), by = c("site" = "Site"))
datalong <- read.csv("model_data_mean_long.csv")

obs.mods <- ggplot(datalong, aes(make_pct(Cplant), make_pct(Csoil), colour=type)) +
  geom_smooth(data=data,aes(make_pct(Cplant), make_pct(Csoil)), method = "lm", se=FALSE, color="#e41a1c", formula = y ~ x) +
  stat_smooth(data=data,aes(make_pct(biomass), make_pct(soil)), method = "lm", se=FALSE, color="#377eb8", formula = y ~ poly(x, 3)) + # orthogonal polynomials, in this case with a 3rd order
  geom_errorbar(aes(ymin=make_pct(Csoil)-make_pct(SE.soil), ymax=make_pct(Csoil)+make_pct(SE.soil)), width=2,colour="black") +
  geom_point(aes(shape=Site),size=3,fill="white") +
  scale_shape_manual(values=c(21:25,4)) + guides(colour=guide_legend(title=NULL)) +
  scale_colour_manual(values=c("#e41a1c","#377eb8")) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  #lims(y=c(make_pct(min(data$soil)),make_pct(max(data$soil))), x=c(make_pct(min(data$biomass)),make_pct(max(data$biomass)))) +
  theme_cowplot(font_size=12) 
save_plot("graphs/ModelsVSObs_1panel.png",obs.mods,type = "cairo-png", base_aspect_ratio = 1.4)


models.p <- ggplot(data, aes(make_pct(Cplant), make_pct(Csoil))) + 
  geom_point(data=models,aes(colour=model,shape=site), size=3) + 
  #geom_point(aes(shape=Site),size=3) +
  #geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  stat_smooth(geom='line',data=models,aes(make_pct(Cplant), make_pct(Csoil),colour=model, group=model), method = "lm", se=FALSE) +
  scale_shape_manual(values=c(15,16,0,1,2,17)) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  guides(col = guide_legend(title = "Model",order = 1),
         shape = guide_legend(title = "Experiment",order = 0)) + 
  #geom_hline(yintercept = 0, lty=2, size=1) + 
  #geom_vline(xintercept = 0, lty=2, size=1) + 
  #stat_poly_eq(formula = y ~ x, hjust = -.2,
   #            aes(label = paste(
                 #..eq.label.., 
                 #stat(adj.rr.label), sep = "~~~")), parse = TRUE) +
  coord_cartesian(ylim = c(make_pct(min(data$soil)),make_pct(max(data$soil))),
                  xlim=c(make_pct(min(data$biomass)),make_pct(max(data$biomass)))) +
  theme_cowplot(font_size=10) +
  theme(legend.text=element_text(size=10),
        legend.title=element_text(size=10),
    #legend.key.size = unit(.7, "lines"),
    #legend.position = c(.55, .88),
    #legend.justification = c("left", "top"),
    #legend.box = "horizontal",
    #legend.box.just = "top",
    legend.spacing.y = unit(1, "pt"),
    legend.margin = margin(6, 6, 6, 6)
  )
models.p
save_plot("graphs/Models.png",models.p,type = "cairo-png", base_aspect_ratio = 1.4)

obs.p <- ggplot(data, aes(make_pct(biomass), make_pct(soil))) +
  geom_point(aes(shape=Site),size=3) +
  guides(shape=FALSE) + 
  #geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  stat_smooth(method = "lm", se=FALSE, color="black", formula = y ~ poly(x, 3)) + # orthogonal polynomials, in this case with a 3rd order
  scale_shape_manual(values=c(15,16,0,1,2,17)) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  #geom_hline(yintercept = 0, lty=2, size=1) + 
  #geom_vline(xintercept = 0, lty=2, size=1) + 
  lims(y=c(make_pct(min(data$soil)),make_pct(max(data$soil))), x=c(make_pct(min(data$biomass)),make_pct(max(data$biomass)))) +
  #stat_poly_eq(formula = y ~ x,  label.x = .95, label.y = .95,aes(label = paste(..eq.label.., stat(adj.rr.label), sep = "~~~")), parse = TRUE) +
  stat_poly_eq(formula = y ~ poly(x, 3),hjust = -.2,
               #label.x = .95, label.y = .85, 
               aes(label = paste(
    #..eq.label.., 
    stat(adj.rr.label), sep = "~~~")), parse = TRUE) +
  theme_cowplot(font_size=14) 

modobs.p <-  plot_grid(models.p, obs.p,
                       align="hv", axis="l",
                       labels = "AUTO",
                       ncol=1)
prow <-  plot_grid(models.p +theme(legend.position="none"), obs.p,
                       align="hv",
                       labels = "AUTO",
                       ncol=1)
legend <- get_legend(models.p + theme(legend.box.margin = margin(0, 5, 0, 5)))
modobs.p <-plot_grid(prow, legend, rel_widths = c(3, .6),ncol=2)
save_plot("graphs/ModelsVSObs.png",modobs.p, nrow=2, type = "cairo-png", base_aspect_ratio = 1.4)

