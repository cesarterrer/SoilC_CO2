library(ggplot2)
library(tidyr)
library(cowplot)
library(ggpmisc)
library(dplyr)
require(mgcv)
require(metafor)
make_pct <- function(x) (exp(x) - 1) * 100
data <- read.csv("model_data_mean.csv")
summary(lm(Csoil~Cabove, data=data))
summary(lm(soil~biomass, data=data))
models <- read.csv("models.csv")
model_data <- left_join(dplyr::select(models, -Cplant),dplyr::select(data,Site,Cplant), by = c("site" = "Site"))
datalong <- read.csv("model_data_mean_long.csv")

#### TRENDY ####
load("gdf.RData")
set.seed(1111)
data.trendy <- gdf %>%
  ## sample 1957 points from each model (corresponds to the number of gridcell of the model with coarsest res.)
  mutate(data = purrr::map(data, ~slice_sample(., n = 1957))) %>% # smallest set here
  unnest(data) %>%
  mutate(modl = "ALL") %>% dplyr::filter(dcsoil_star < 2 & dcsoil_star > -2 & dcveg_ag < 2 & dcveg_ag > -2) %>% 
  dplyr::filter(!is.nan(dnpp), !is.nan(dcveg_ag), !is.infinite(dnpp), !is.infinite(dcveg_ag))
p.trendy <- ggplot(data.trendy,aes(x = make_pct(dcveg_ag), y = make_pct(dcsoil_star))) +
  stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") + 
  theme_cowplot(font_size=8) +
  #geom_smooth(size=.5, method = "lm", se=FALSE, color="#e41a1c", formula = y ~ x) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_vline(xintercept = 0, color = "grey50", size = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 0.2) +
  scale_fill_gradientn(colours = colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),
                       name="Density") +
  # facet_wrap(. ~ modl, nrow = 3,  scales = "free") +
  coord_cartesian(ylim = c(make_pct(min(data$soil))-5,make_pct(max(data$soil))+5),
                  xlim=c(make_pct(min(data$biomass)),make_pct(max(data$biomass)))) +
  #labs(x = expression(paste(Delta, "C"[ag-veg], "/C"[ag-veg])),
   #    y = expression(paste(Delta, "C"[soil], "/C"[soil]))) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  theme(strip.background = element_blank(), legend.key.width= unit(.1, "cm"), legend.key.height= unit(.2, "cm"), 
        legend.text=element_text(size=6),legend.position = c(0.85, 0.15))
p.trendy

#### FACE-MDS ####

obs.mods <- ggplot(datalong, aes(make_pct(Cabove), make_pct(Csoil), colour=type)) +
  geom_smooth(size=.5,data=filter(datalong,type=="modeled"), method = "lm", se=FALSE, color="#e41a1c", formula = y ~ x) +
  stat_smooth(size=.5,data=filter(datalong,type=="observed"), method = "lm", se=FALSE, color="#377eb8", formula = y ~ poly(x, 3)) + # orthogonal polynomials, in this case with a 3rd order
  geom_errorbar(aes(ymin=make_pct(Csoil)-make_pct(SE.soil), ymax=make_pct(Csoil)+make_pct(SE.soil)), width=1,size=.3,colour="black") +
  geom_point(aes(shape=Site),size=1.8,fill="white",alpha=.7) +
  scale_shape_manual(values=c(21:25,4)) + guides(colour=guide_legend(title=NULL)) +
  scale_colour_manual(values=c("#e41a1c","#377eb8")) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_vline(xintercept = 0, color = "grey50", size = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 0.2) +
  guides(col = guide_legend(title = NULL,order = 0),
         shape = guide_legend(title = "Experiment",order = 1)) + 
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  coord_cartesian(ylim = c(make_pct(min(data$soil))-5,make_pct(max(data$soil))+5),
                  xlim=c(make_pct(min(data$biomass)),make_pct(max(data$biomass)))) +
  theme_cowplot(font_size=8) +
  theme(legend.text=element_text(size=6),legend.position = c(0.75, 0.7),
        legend.spacing.y = unit(2, "pt"))
obs.mods
save_plot("graphs/ModelsVSObs_1panel.png",obs.mods,type = "cairo-png", base_width = 2.5, base_height = 2.25, dpi=1200)

# 2 Panels
p <- plot_grid(p.trendy + theme(plot.margin = unit(c(5,-5,0,5), "points")), 
                        obs.mods + theme(plot.margin = unit(c(5,5,0,-5), "points")) + ylab(""),
                        align = 'hv', label_size=10,
                        labels = c("a", "b"),
                        vjust = 1.2, hjust = -0.5,
                        nrow = 1)

save_plot("graphs/ModelsVSObs_2panel.png",p, ncol=2, nrow=1, type = "cairo-png", base_width = 2.75, base_height = 2.5, dpi=1200)


models.p <- ggplot(data, aes(make_pct(Cabove), make_pct(Csoil))) + 
  geom_point(data=models,aes(colour=model,shape=site), size=3) + 
  #geom_point(aes(shape=Site),size=3) +
  #geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  stat_smooth(geom='line',data=models,aes(make_pct(Cabove), make_pct(Csoil),colour=model, group=model), method = "lm", se=FALSE) +
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
save_plot("graphs/Models.png",models.p,type = "cairo-png", base_aspect_ratio = 1.4)
