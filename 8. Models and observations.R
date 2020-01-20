library(ggplot2)
library(tidyr)
library(cowplot)
library(ggpmisc)
library(dplyr)
require(mgcv)
make_pct <- function(x) (exp(x) - 1) * 100

data <- read.csv("model_data_mean.csv") %>% mutate(soil.se=sqrt(var))
models <- read.csv("models.csv")
model_data <- left_join(dplyr::select(models, -Cplant),dplyr::select(data,Site,Cplant), by = c("site" = "Site"))

models.p <- ggplot(data, aes(make_pct(Cplant), make_pct(Csoil))) + 
  #geom_point(data=model_data,aes(colour=model), alpha=0.3) + 
  geom_point(data=models,aes(colour=model,shape=site), alpha=0.3,size=3) + 
  geom_point(aes(shape=Site),size=3) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  stat_smooth(geom='line',data=models,aes(make_pct(Cplant), make_pct(Csoil),colour=model, group=model), alpha=0.3,method = "lm", se=FALSE) +
  scale_shape_manual(values=c(15,16,0,1,2,17)) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  guides(col = guide_legend(title = "Model",order = 0),
         shape = guide_legend(title = "Experiment",order = 1)) + 
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_vline(xintercept = 0, lty=2, size=1) + 
  stat_poly_eq(formula = y ~ x, hjust = -.2,
               aes(label = paste(..eq.label.., stat(adj.rr.label), sep = "~~~")), 
               parse = TRUE) +
  lims(y=c(make_pct(min(data$soil)),make_pct(max(data$soil))), x=c(make_pct(min(data$biomass)),make_pct(max(data$biomass)))) +
  #lims(y=c(-5,30), x=c(0,31)) +
  theme_cowplot(font_size=12) +
  theme(legend.text=element_text(size=8), legend.key.size = unit(.7, "lines"),
    legend.position = c(.55, .88),
    legend.justification = c("left", "top"),
    legend.box = "horizontal",
    legend.box.just = "top",
    legend.spacing.y = unit(1, "pt"),
    legend.margin = margin(6, 6, 6, 6)
  )
models.p
obs.p <- ggplot(data, aes(make_pct(biomass), make_pct(soil))) +
  geom_point(aes(shape=Site),size=3) +
  guides(shape=FALSE) + 
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  stat_smooth(method = "lm", se=FALSE, color="black", formula = y ~ poly(x, 3),linetype = "dashed") + # orthogonal polynomials, in this case with a 3rd order
  scale_shape_manual(values=c(15,16,0,1,2,17)) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  geom_hline(yintercept = 0, lty=2, size=1) + 
  geom_vline(xintercept = 0, lty=2, size=1) + 
  lims(y=c(make_pct(min(data$soil)),make_pct(max(data$soil))), x=c(make_pct(min(data$biomass)),make_pct(max(data$biomass)))) +
  stat_poly_eq(formula = y ~ x,  label.x = .95, label.y = .95,aes(label = paste(..eq.label.., stat(adj.rr.label), sep = "~~~")), parse = TRUE) +
  stat_poly_eq(formula = y ~ poly(x, 3),label.x = .95, label.y = .85, aes(label = paste(..eq.label.., stat(adj.rr.label), sep = "~~~")), parse = TRUE) +
  theme_cowplot(font_size=12) 

modobs.p <-  plot_grid(models.p, obs.p,
                       align="hv",
                       labels = "AUTO",
                       ncol=1)
save_plot("graphs/ModelsVSObs.png",modobs.p, ncol=1, nrow=2, type = "cairo-png", base_aspect_ratio = 1.3)

