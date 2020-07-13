library(devtools)
library(ggplot2)
library(raster)
library(RColorBrewer)
library(grid)
library(ggplot2)
library(dplyr)
library(rgdal)
library(cowplot)
library(scales)
library(viridis)
library(swatches)
devtools::install_github("hrbrmstr/ggalt")
library(ggalt)
library(nord)
library(ggpolypath)
library(Cairo)
library(colorspace)
library(cptcity) # http://soliton.vm.bytemark.co.uk/pub/cpt-city/index.html
library(rgeos)
require(Cairo)
library(ggspatial)
library(rcartocolor)
library(colorspace)
library(tibble)
library(mapproj)
#library(sf)
options(bitmapType="cairo")
nord <- read_palette("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/nord.ase")
# map the bbox
bbox <- shapefile("maps/ne_110m_wgs84_bounding_box.VERSION/ne_110m_wgs84_bounding_box.shp") 
bbox <- spTransform(bbox, CRS("+proj=robin"))
bbox_df<- fortify(bbox)

# Coastlines
wmap <- readOGR(dsn="maps/ne_50m_land", layer="ne_50m_land")
# http://www.naturalearthdata.com/downloads/
wmap_wgs <- spTransform(wmap, CRS("+proj=robin"))
wmap_wgs_df <- fortify(wmap_wgs)

# Ocean
# http://www.naturalearthdata.com/downloads/
ocean <- readOGR(dsn="maps/ne_50m_ocean", layer="ne_50m_ocean")
ocean <- spTransform(ocean, CRS("+proj=robin"))
oceanmap_df <- fortify(ocean)

# graticule (Robin)
grat <- shapefile("maps/ne_110m_graticules_30.VERSION/ne_110m_graticules_30.shp") 
grat<- spTransform(grat, CRS("+proj=robin"))  # reproject graticule
grat_df <- fortify(grat)

# create a blank ggplot theme
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         #plot.background = element_rect(fill="#e6e8ed"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22),
                         plot.margin = unit(c(.5,-.5,.5,-.5), "lines"), 
                         legend.title = element_blank(),
                         axis.title.x = element_text(margin = margin(t = -5, r = 0, b = 0, l = 0)),
                         legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="lines"), 
                         legend.position="bottom", 
                         legend.direction = "horizontal",
                         legend.key.width = unit(.7, "cm"), 
                         legend.key.height = unit(0.1, "cm"),
                         legend.text = element_text(margin = margin(t = .1, unit = "lines"))
))

discrete_gradient_pal <- function(colours, bins = 11) {
  ramp <- scales::colour_ramp(colours)
  
  function(x) {
    if (length(x) == 0) return(character())
    
    i <- floor(x * bins)
    i <- ifelse(i > bins-1, bins-1, i)
    ramp(i/(bins-1))
  }
}

scale_fill_discrete_gradient <- function(..., colours, bins = 11, na.value = "transparent", guide = "colourbar", aesthetics = "fill", colors)  {
  colours <- if (missing(colours)) 
    colors
  else colours
  continuous_scale(
    aesthetics,
    "discrete_gradient",
    discrete_gradient_pal(colours, bins),
    na.value = na.value,
    guide = guide,
    ...
  )
}
# -----------------------------------------------------------------------
# ABSOLUTE EFFECT
abs <- raster("maps/CO2abslEffect_RF_tha.tif")  # Mg/ha
abs_robin <- projectRaster(abs,crs="+proj=robin",over=T)
abs.df = as.data.frame(abs_robin,xy=TRUE)
range(abs.df$CO2abslEffect_RF_tha, na.rm=T)

find_cpt("temperature")
mycols <- cpt(pal = "arendal_temperature", rev=T, n=13)
mycols <- mycols[c(2,4:13)]
show_col(mycols)

p <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=abs.df,aes(x,y,fill=CO2abslEffect_RF_tha)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    bins = 11,
    colours = mycols,
    #breaks=-3:11,
    breaks=-1:10,
    limits = c(-1, 10),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab(expression(paste("Change in soil C stock  (Mg ", ha^-1,")", " in response to ", CO[2] ,sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 8) + 
  theme_opts

#save_plot("graphs/CO2absEffect.pdf", p, base_aspect_ratio = 1.5, dpi=300)
#save_plot("graphs/CO2absEffect.png", p, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

### PERCENTAGE ###
perc <- raster("maps/CO2relEffect_RF_tha.tif")
perc_robin <- projectRaster(perc,crs="+proj=robin",over=T)
perc.df = as.data.frame(perc_robin,xy=TRUE)
range(perc.df$CO2relEffect_RF_tha, na.rm=T)
mycols.perc <- cpt(pal = "arendal_temperature", rev=T, n=25)
show_col(mycols.perc)
mycols.perc <- mycols.perc[c(3,6:25)]

p.perc <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=perc.df,aes(x,y,fill=CO2relEffect_RF_tha)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    bins = 21,
    colours = mycols.perc,
    breaks=seq(0,20,5),
    limits = c(-1, 20),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab(expression(paste("Change in soil C stock (%) in response to ", CO[2] ,sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 8) + 
  theme_opts
p.perc
#save_plot("graphs/CO2relEffect.pdf", p.perc, base_aspect_ratio = 1.5, dpi=300)
#save_plot("graphs/CO2relEffect.png", p.perc, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

### FACE - EXPECTED ###
source("CMIP.R")
# Absolute
dif.abs <- raster("maps/Diff_obs_exp_tha.tif")
dif.abs_robin <- projectRaster(dif.abs,crs="+proj=robin",over=T)
dif.abs.df = as.data.frame(dif.abs_robin,xy=TRUE)
range(dif.abs.df$Diff_obs_exp_tha, na.rm=T)

find_cpt("BlueYellowRed")
difcol <- cpt(pal = "ncl_BlueYellowRed", rev=T, n=20)
difcol <- cpt(pal = "arendal_temperature", rev=T, n=10)
difcol <- cpt(pal = "ncl_BlueDarkRed18", rev=T, n=24)
#show_col(difcol)

p.dif.abs <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=dif.abs.df,aes(x,y,fill=Diff_obs_exp_tha)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    bins = 24,
    colours = difcol,
    breaks=seq(-10,10,5),
    limits = c(-12, 12),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab(expression(paste("(Upscaled - Expected) change in soil C (Mg ", ha^-1,")",sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 8) + 
  theme_opts
#save_plot("graphs/Diff_obs_exp_tha.pdf", p.dif.abs, base_aspect_ratio = 1.5, dpi=300)
#save_plot("graphs/Diff_obs_exp_tha.png", p.dif.abs, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

# Relative
dif.rel <- raster("maps/Diff_obs_exp_perc.tif")
dif.rel_robin <- projectRaster(dif.rel,crs="+proj=robin",over=T)
dif.rel.df = as.data.frame(dif.rel_robin,xy=TRUE)
range(dif.rel.df$Diff_obs_exp_perc, na.rm=T)

difcol.rel <- cpt(pal = "ncl_BlueDarkRed18", rev=T, n=36)
p.dif.rel <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=dif.rel.df,aes(x,y,fill=Diff_obs_exp_perc)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    bins = 36,
    colours = difcol.rel,
    breaks=seq(-15,15,5),
    limits = c(-18, 18),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab("(Upscaled - Expected) change in soil C (%)") +
  coord_equal() + 
  theme_classic(base_size = 8) + 
  theme_opts
#save_plot("graphs/Diff_rel_exp_tha.pdf", p.dif.rel, base_aspect_ratio = 1.5, dpi=300)
#save_plot("graphs/Diff_rel_exp_tha.png", p.dif.rel, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

# ------------------------------------------------------------------------
### UNCERTAINTIES ###
# Absolute Error
se <- raster("maps/CO2abslEffect.SE_RF_tha.tif")  # Mg/ha
se_robin <- projectRaster(se,crs="+proj=robin",over=T)
se.df = as.data.frame(se_robin,xy=TRUE)
range(se.df$CO2abslEffect.SE_RF_tha, na.rm=T)

uncert.col <- plasma(n=20, direction=-1)
uncert.col.cequal <- cpt(pal = "imagej_cequal", rev=T, n=12)
show_col(uncert.col.cequal)
error <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=se.df,aes(x,y,fill=CO2abslEffect.SE_RF_tha)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    bins = 12,
    colours = uncert.col.cequal,
    #breaks=-3:11,
    breaks=seq(0,2.5,0.5),
    limits = c(0, 3),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab(expression(paste("Uncertainty in soil C estimate (Mg ", ha^-1,")",sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 8) + 
  theme_opts
#save_plot("graphs/CO2absEffect_uncertainties.pdf", error, base_aspect_ratio = 1.5, dpi=300)
#save_plot("graphs/CO2absEffect_uncertainties.png", error, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

# Percentage error
perc.se <- raster("maps/CO2relEffect.SE_RF_tha.tif")
perc.se_robin <- projectRaster(perc.se,crs="+proj=robin",over=T)
perc.se.df = as.data.frame(perc.se_robin,xy=TRUE)
range(perc.se.df$CO2relEffect.SE_RF_tha, na.rm=T)
uncert.col.rel <- plasma(n=20, direction=-1)
uncert.col.rel.cequal<- cpt(pal = "imagej_cequal", rev=T, n=20)
show_col(uncert.col.se)

error.perc <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=perc.se.df,aes(x,y,fill=CO2relEffect.SE_RF_tha)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    bins = 20,
    colours = uncert.col.rel.cequal,
    #breaks=-3:11,
    breaks=seq(0,5,1),
    limits = c(0, 5),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab("Uncertainty in soil C estimate (%)") +
  coord_equal() + 
  theme_classic(base_size = 8) + 
  theme_opts
save_plot("graphs/CO2relEffect_uncertainties.pdf", error.perc, base_aspect_ratio = 1.5, dpi=300)
save_plot("graphs/CO2relEffect_uncertainties.png", error.perc, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)


# -------------------------------------------------------------------------
### BOXPLOT ###
library(tidyverse)
esa <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_2012_aggregated0p25.tif")
#esa_pan <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_PAN.tif")
legend <- read.csv("ESA_classes.csv")
s.eco <- stack(esa, abs, perc, dif.abs, dif.rel, se, perc.se)
eco.df <- as.data.frame(s.eco)
names(eco.df) <- c("ESA", "abs", "perc", "dif.abs", "dif.rel", "se", "se.rel")
eco.df <- left_join(eco.df,legend, by=c("ESA" = "NB_LAB")) %>% drop_na()

box.abs <- ggplot(data=eco.df,aes(ESAagg, abs)) + 
  geom_boxplot(fatten = NULL, show.legend = F,outlier.alpha = 0.1) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1, linetype = "solid") +
  scale_fill_brewer(palette="Set3") +
  ylab(expression(paste(eCO[2], " effect on soil C (Mg ", ha^-1,")" ,sep="")))+ xlab("") +
  theme_classic(base_size = 8) +
  theme(axis.text.x = element_text(size=8,angle = 35, hjust = 1))
box.rel <- ggplot(data=eco.df,aes(ESAagg, perc)) + 
  geom_boxplot(fatten = NULL, show.legend = F,outlier.alpha = 0.1) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1, linetype = "solid") +
  scale_fill_brewer(palette="Set3") + 
  ylab(expression(paste(eCO[2], " effect on soil C (%)" ,sep="")))+ xlab("") +
  theme_classic(base_size = 8) +
  theme(axis.text.x = element_text(size=8,angle = 35, hjust = 1))
box.dif.abs <- ggplot(data=eco.df,aes(ESAagg, dif.abs)) + 
  geom_boxplot(fatten = NULL, show.legend = F,outlier.alpha = 0.1) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1, linetype = "solid") +
  scale_fill_brewer(palette="Set3") + 
  geom_hline(yintercept=0, linetype="dashed") +
  ylab(expression(paste("(Upscaled - Expected) change in soil C (Mg ", ha^-1,")",sep=""))) + xlab("") +
  theme_classic(base_size = 8) +
  theme(axis.text.x = element_text(size=8,angle = 35, hjust = 1))
box.dif.rel <- ggplot(data=eco.df,aes(ESAagg, dif.rel)) + 
  geom_boxplot(fatten = NULL, show.legend = F,outlier.alpha = 0.1) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1, linetype = "solid") +
  scale_fill_brewer(palette="Set3") + 
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("(Upscaled - Expected) change in soil C (%)") + xlab("") +
  theme_classic(base_size = 8) +
  theme(axis.text.x = element_text(size=8,angle = 35, hjust = 1))
box.uncert.abs <- ggplot(data=eco.df,aes(ESAagg, se)) + 
  geom_boxplot(fatten = NULL, show.legend = F,outlier.alpha = 0.1) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1, linetype = "solid") +
  scale_fill_brewer(palette="Set3") + 
  ylab(expression(paste("Uncertainty (Mg ", ha^-1,")",sep=""))) + xlab("") +
  theme_classic(base_size = 8) +
  theme(axis.text.x = element_text(size=8,angle = 35, hjust = 1))
box.uncert.rel <- ggplot(data=eco.df,aes(ESAagg, se.rel)) + 
  geom_boxplot(fatten = NULL, show.legend = F,outlier.alpha = 0.1) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = 0.75, size = 1, linetype = "solid") +
  scale_fill_brewer(palette="Set3") + 
  ylab(expression(paste("Uncertainty (Mg ", ha^-1,")",sep=""))) + xlab("") +
  theme_classic(base_size = 8) +
  theme(axis.text.x = element_text(size=8,angle = 35, hjust = 1))

# -------------------------------------------------------------------------
# FIGURE 3
boxes <- plot_grid( box.rel + theme(plot.margin = unit(c(0.2,0.1,-1,0), "cm")),
                    box.dif.rel + theme(plot.margin = unit(c(0,0.1,0,0), "cm")) + ylab("Upscaled - Expected (%)"),
                    box.uncert.abs + theme(plot.margin = unit(c(-1,0.1,0,0), "cm")), 
                    align = 'vh', 
                    label_size = 10, 
                    labels = c("b","d","f"),
                    hjust = 2, 
                    #vjust= 4,
                    nrow = 3,
                    ncol=1)

maps <- plot_grid( p.perc + theme(plot.margin = unit(c(0,-.5,-1,-.5), "cm")) + xlab(expression(paste("Upscaled ",eCO[2], " effect on soil C (%)" ,sep=""))), 
                   p.dif.rel + theme(plot.margin = unit(c(0,-.5,0,-.5), "cm")) ,
                   error + theme(plot.margin = unit(c(-1,-.5,0,-.5), "cm")),
                   align = 'h', 
                   label_size = 10,
                   labels = c("a","c","e"),
                   hjust = -3, 
                   #vjust= 4,
                   nrow = 3,
                   ncol=1
)

fig3 <- plot_grid(maps+ theme(plot.margin = unit(c(0,0,0,0), "cm")),
                  boxes + theme(plot.margin = unit(c(0,0,0,0), "cm")),
                  labels =NULL ,
                  rel_widths = c(1, .4),
                  nrow = 1, ncol=2)
save_plot("graphs/Fig3.png", fig3, dpi=1200, ncol=2, nrow=1, base_height = 6, base_width = 2.25,type = "cairo-png")
save_plot("graphs/Fig3.pdf", fig3, dpi=600, base_width = 210/2, base_height = ((3/5)*210)/2, units="mm",nrow=2, ncol = 1, device = cairo_pdf, fallback_resolution = 1200)
save_plot("graphs/Fig3.eps", fig3, dpi=600, base_width = 210/2, base_height = ((3/5)*210)/2, units="mm",nrow=2, ncol = 1, device = cairo_ps, fallback_resolution = 1200)
