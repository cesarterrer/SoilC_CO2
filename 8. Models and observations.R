library(ggplot2)
library(tidyr)
library(cowplot)
library(ggpmisc)
library(dplyr)
require(mgcv)
require(metafor)
make_pct <- function(x) (exp(x) - 1) * 100
#### FACE-MDS ####
data <- read.csv("model_data_mean.csv")
summary(lm(Csoil~Cabove, data=data))
summary(lm(soil~biomass + I(biomass^2), data=data))
models <- read.csv("models.csv")
model_data <- left_join(dplyr::select(models, -Cplant),dplyr::select(data,Site,Cplant), by = c("site" = "Site"))
datalong <- read.csv("model_data_mean_long.csv")

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
#save_plot("graphs/ModelsVSObs_1panel.png",obs.mods,type = "cairo-png", base_width = 2.5, base_height = 2.25, dpi=1200)

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
  #stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") + 
  stat_density_2d(aes(fill = after_stat(ndensity)),contour = FALSE, geom = "raster",n=5000) +
  theme_cowplot(font_size=8) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_vline(xintercept = 0, color = "grey50", size = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 0.2) +
  scale_fill_gradientn(colours = colorRampPalette( c("white","gray65", "navy", "red", "yellow"))(5),
                       name="Density") +
  coord_cartesian(y = c(make_pct(min(data$soil))-5,make_pct(max(data$soil))+5),
                  x=c(make_pct(min(data$biomass)),make_pct(max(data$biomass)))) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  theme(strip.background = element_blank(), legend.key.width= unit(.1, "cm"), legend.key.height= unit(.2, "cm"), 
        legend.text=element_text(size=6),legend.position = c(0.85, 0.15))
p.trendy
#save_plot("graphs/trendy.png",p.trendy,type = "cairo-png", base_width = 2.5, base_height = 2.25, dpi=1200)


#### CMIP5 ####
load("gdf_CMIP5.rda")
set.seed(1111)
data.cmip <- gdf %>%
  ## sample 1957 points from each model (corresponds to the number of gridcell of the model with coarsest res.)
  mutate(data = purrr::map(data, ~slice_sample(., n = 1957))) %>% # smallest set here
  unnest(data) %>%
  mutate(modl = "ALL") %>% 
  dplyr::filter(dcsoil_star < 2 & dcsoil_star > -2 & dcveg_ag < 2 & dcveg_ag > -2) %>% 
  dplyr::filter(!is.nan(dnpp), !is.nan(dcveg_ag), !is.infinite(dnpp), !is.infinite(dcveg_ag))

p.cmip <-ggplot(data.cmip, aes(x = make_pct(dcveg_ag), y = make_pct(dcsoil_star))) +
  #stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") + 
  stat_density_2d(aes(fill = after_stat(ndensity)),contour = FALSE, geom = "raster",n=5000) +
  theme_cowplot(font_size=8) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_vline(xintercept = 0, color = "grey50", size = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 0.2) +
  scale_fill_gradientn(colours = colorRampPalette( c("white","gray65", "navy", "red", "yellow"))(5),
                       name="Density") +
  coord_cartesian(y = c(make_pct(min(data$soil))-5,make_pct(max(data$soil))+5),
       x=c(make_pct(min(data$biomass)),make_pct(max(data$biomass)))) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  theme(strip.background = element_blank(), legend.key.width= unit(.1, "cm"), legend.key.height= unit(.2, "cm"), 
        legend.text=element_text(size=6),legend.position = c(0.85, 0.15))
p.cmip


# 3 Panels
p <- plot_grid(obs.mods + theme(plot.margin = unit(c(5,-5,0,5), "points")), 
               p.trendy + theme(plot.margin = unit(c(5,5,0,-5), "points")) + ylab("") + theme(legend.position = "none"),
               p.cmip + theme(plot.margin = unit(c(5,5,0,-5), "points")) + ylab(""),
                        align = 'hv', label_size=10,
                        labels = "auto",
                        vjust = 1.2, hjust = -0.5,
                        nrow = 1)

save_plot("graphs/ModelsVSObs_2panel.png",p, ncol=3, nrow=1, type = "cairo-png", base_width = 2.75, base_height = 2.5, dpi=1200, bg="white")


######################### INDIVIDUAL MODELS ##########################

###### FACE MDS #####
summary(lm(Csoil~Cabove, data=models))
face.ind <- ggplot(data, aes(make_pct(Cabove), make_pct(Csoil))) + 
  geom_point(data=models,aes(colour=model,shape=site), size=3) + 
  stat_smooth(data=models,method = "lm",col="black", size=1, linetype="dashed") +
  #geom_point(aes(shape=Site),size=3) +
  #geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  stat_smooth(geom='line',data=models,aes(make_pct(Cabove), make_pct(Csoil),colour=model, group=model), method = "lm", se=FALSE) +
  scale_shape_manual(values=c(15,16,0,1,2,17)) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  guides(col = guide_legend(title = "Model",order = 1),
         shape = guide_legend(title = "Experiment",order = 0)) + 
  coord_cartesian(ylim = c(-10,100),xlim=c(0,100)) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_vline(xintercept = 0, color = "grey50", size = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 0.2) +
  theme_cowplot(font_size=12) +
  theme(legend.text=element_text(size=8),
        legend.title=element_text(size=8),
    legend.spacing.y = unit(1, "pt"),
    legend.margin = margin(6, 6, 6, 6)
  )
face.ind 
save_plot("graphs/Models.png",face.ind,type = "cairo-png", base_aspect_ratio = 1.4)

##### TRENDY #####
load("gdf.RData")
set.seed(1)
trendy.ind <- gdf %>%
  unnest(data) %>%
  dplyr::filter(dcsoil_star < 2 & dcsoil_star > -2 & dcveg_ag < 2 & dcveg_ag > -2) %>%
  dplyr::filter(!is.nan(dnpp), !is.nan(dcveg_ag), !is.infinite(dnpp), !is.infinite(dcveg_ag)) %>%
  ggplot(aes(x = make_pct(dcveg_ag), y = make_pct(dcsoil_star))) +
  stat_density_2d(aes(fill = after_stat(ndensity)),contour = FALSE, geom = "raster") +
  stat_smooth(size=.5, method = "lm", color="#377eb8") +
  theme_cowplot(font_size=12) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_vline(xintercept = 0, color = "grey50", size = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 0.2) +
  scale_fill_gradientn(colours = colorRampPalette( c("white","gray65", "navy", "red", "yellow"))(5),
                       guide = "legend") +
  facet_wrap(. ~ modl, nrow = 2,  scales = "free") +
  ylim(-10,100) + xlim(0,100) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  theme(legend.position = "none",
        strip.background = element_blank())
trendy.ind

####### CMIP5 ########
load("gdf_CMIP5.rda")
cmip.ind <- gdf %>%
  unnest(data) %>%
  dplyr::filter(dcsoil_star < 2 & dcsoil_star > -2 & dcveg_ag < 2 & dcveg_ag > -2) %>% 
  dplyr::filter(!is.nan(dcveg_ag), !is.infinite(dcveg_ag)) %>% 
  ggplot(aes(x = make_pct(dcveg_ag), y = make_pct(dcsoil_star))) +
  stat_density_2d(aes(fill = after_stat(ndensity)),contour = FALSE, geom = "raster") +
  stat_smooth(size=.5, method = "lm", color="#377eb8") +
  facet_wrap(. ~ modl, nrow = 2,  scales = "free") + 
  theme_cowplot(font_size=12) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_vline(xintercept = 0, color = "grey50", size = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", size = 0.2) +
  scale_fill_gradientn(colours = colorRampPalette( c("white","gray65", "navy", "red", "yellow"))(5),
                       guide = "legend") +
  ylim(-10,100) + xlim(0,100) +
  labs(x=expression(paste(CO[2]," effect on biomass carbon (%)", sep="")),
       y=expression(paste(CO[2]," effect on soil carbon (%)", sep=""))) +
  theme(legend.position = "none",
        strip.background = element_blank())
cmip.ind

########### MULTI ###########
ind <- plot_grid(face.ind + theme(legend.key.size = unit(0, 'lines')), 
               trendy.ind,
               cmip.ind,
               align = 'v', label_size=12,
               labels = "auto",
               vjust = 1.2, hjust = -0.5,
               rel_heights = c(1,1,1),
               nrow = 3)

save_plot("graphs/ModelsIndividual.png",ind, base_asp=1.8, ncol=1, nrow=3, type = "cairo-png", dpi=1200, bg="white")

