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

# Absolute Effect
abs <- raster("maps/CO2abslEffect_RF_tha.tif")  # Mg/ha
abs <- projectRaster(abs,crs="+proj=robin",over=T)
abs.df = as.data.frame(abs,xy=TRUE)
range(abs.df$CO2abslEffect_RF_tha, na.rm=T)

# Absolute Error
se <- raster("maps/CO2abslEffect.SE_RF_tha.tif")  # Mg/ha
se <- projectRaster(se,crs="+proj=robin",over=T)
se.df = as.data.frame(se,xy=TRUE)
range(se.df$CO2abslEffect.SE_RF_tha, na.rm=T)

# map the bbox
bbox <- shapefile("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ne_110m_wgs84_bounding_box.VERSION/ne_110m_wgs84_bounding_box.shp") 
bbox <- spTransform(bbox, CRS("+proj=robin"))
bbox_df<- fortify(bbox)

# Coastlines
wmap <- readOGR(dsn="~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ne_50m_land", layer="ne_50m_land")
# http://www.naturalearthdata.com/downloads/
wmap_wgs <- spTransform(wmap, CRS("+proj=robin"))
wmap_wgs_df <- fortify(wmap_wgs)

# Ocean
# http://www.naturalearthdata.com/downloads/
ocean <- readOGR(dsn="~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ne_50m_ocean", layer="ne_50m_ocean")
ocean <- spTransform(ocean, CRS("+proj=robin"))
oceanmap_df <- fortify(ocean)

# graticule (Robin)
grat <- shapefile("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ne_110m_graticules_30.VERSION/ne_110m_graticules_30.shp") 
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
                         axis.title.x = element_text(margin = margin(t = -10, r = 0, b = 0, l = 0)),
                         legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="lines"), 
                         legend.position="bottom", 
                         legend.direction = "horizontal",
                         legend.key.width = unit(1, "cm"), 
                         legend.key.height = unit(0.2, "cm"),
                         legend.text = element_text(margin = margin(t = .2, unit = "lines"))
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

find_cpt("temperature")
#myinterval <- seq(-3,11,1)
#abs.df$colbreaks <- findInterval(abs.df$CO2abslEffect_RF_tha, vec = myinterval)
mycols <- cpt(pal = "arendal_temperature", rev=T, n=12)
#mycols <- mycols[c(2,4:12)]
mycols <- mycols[2:12]
#mycols[4] <- "#ffffff"
show_col(mycols)

p <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=abs.df,aes(x,y,fill=CO2abslEffect_RF_tha)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    bins = 10,
    colours = mycols,
    #breaks=-3:11,
    breaks=-1:9,
    limits = c(-1, 9),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab(expression(paste("Change in soil C stock  (Mg ", ha^-1,")", " in response to ", CO[2] ,sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 10) + 
  theme_opts
p

save_plot("graphs/CO2absEffect.pdf", p, base_aspect_ratio = 1.5, dpi=300)
save_plot("graphs/CO2absEffect.png", p, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

# Uncertainties
uncert.col <- plasma(n=10, direction=-1)
uncert.col.cequal <- cpt(pal = "imagej_cequal", rev=T, n=10)
error <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=se.df,aes(x,y,fill=CO2abslEffect.SE_RF_tha)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    bins = 10,
    colours = uncert.col.cequal,
    #breaks=-3:11,
    breaks=seq(0,2.5,0.5),
    limits = c(0, 2.5),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab(expression(paste("Uncertainty in soil estimate (Mg ", ha^-1,")",sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 10) + 
  theme_opts
save_plot("graphs/CO2absEffect_uncertainties.pdf", error, base_aspect_ratio = 1.5, dpi=300)
save_plot("graphs/CO2absEffect_uncertainties.png", error, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

##### ECOSYSTEM C ######
eco<- raster("maps/CO2abslEffect_RF_Ecosystem_tha.tif")  # Mg/ha
eco <- projectRaster(eco,crs="+proj=robin",over=T)
eco.df = as.data.frame(eco,xy=TRUE)
range(eco.df$CO2abslEffect_RF_Ecosystem_tha,na.rm=T)

my_viridis <- c("#FFFFFF", viridis(n=9, direction=-1))
show_col(my_viridis)

p2 <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=eco.df,aes(x,y,fill=CO2abslEffect_RF_Ecosystem_tha)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    bins = 10,
    colours = my_viridis,
    #breaks=-3:11,
    breaks=c(0,10,20,30,40,50),
    limits = c(0, 50),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab(expression(paste("Change in ecosystem C stock  (Mg ", ha^-1,")", " in response to ", CO[2] ,sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 10) + 
  theme_opts
p2

save_plot("graphs/CO2absEffect_Ecosystem.pdf", p2, base_aspect_ratio = 1.5, dpi=300)
save_plot("graphs/CO2absEffect_Ecosystem.png", p2, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

# Uncertainty
eco.se <- raster("maps/CO2abslEffect.SE_RF_Ecosystem_tha.tif")  # Mg/ha
eco.se <- projectRaster(eco.se,crs="+proj=robin",over=T)
eco.se.df = as.data.frame(eco.se,xy=TRUE)
range(eco.se.df$CO2abslEffect.SE_RF_Ecosystem_tha,na.rm=T)
uncert.col2 <- plasma(n=12, direction=-1)
uncert.col3 <- cpt(pal = "imagej_cequal", rev=T, n=8)

p.eco.se <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=eco.se.df,aes(x,y,fill=CO2abslEffect.SE_RF_Ecosystem_tha)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    bins = 8,
    colours = uncert.col3,
    #breaks=-3:11,
    breaks=c(0,2.5,5,7.5,10),
    limits = c(0, 10),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab(expression(paste("Change in ecosystem C stock  (Mg ", ha^-1,")", " in response to ", CO[2] ,sep=""))) +
  xlab(expression(paste("Uncertainty in ecosystem-level estimate (Mg ", ha^-1,")",sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 10) + 
  theme_opts
p.eco.se

# FIGURE 3
fig3 <- plot_grid( p + theme(plot.margin = unit(c(0,-.5,-1,-.5), "cm")) + xlab(expression(paste(eCO[2], " effect on soil C stocks (Mg ", ha^-1,")" ,sep=""))), 
                  p2 + theme(plot.margin = unit(c(-1,-.5,0,-.5), "cm")) + xlab(expression(paste(eCO[2], " effect on ecosystem C stocks (Mg ", ha^-1,")" ,sep=""))), 
                                 align = 'h', 
                                 #label_size = 8,
                                 labels = "AUTO",
                                 hjust = -3, 
                                 #vjust= 4,
                                 nrow = 2,
                                 ncol=1
)

save_plot("graphs/Fig3.png", fig3, dpi=1200, base_width = 210/2, base_height = ((3/5)*210)/2, units="mm", nrow=2, ncol = 1,type = "cairo-png")
save_plot("graphs/Fig3.pdf", fig3, dpi=600, base_width = 210/2, base_height = ((3/5)*210)/2, units="mm",nrow=2, ncol = 1, device = cairo_pdf, fallback_resolution = 1200)
save_plot("graphs/Fig3.eps", fig3, dpi=600, base_width = 210/2, base_height = ((3/5)*210)/2, units="mm",nrow=2, ncol = 1, device = cairo_ps, fallback_resolution = 1200)

# UNCERTAINTIES
uncert.p <- plot_grid( error + theme(plot.margin = unit(c(0,-.5,-1,-.5), "cm")), 
                   p.eco.se + theme(plot.margin = unit(c(-1,-.5,0,-.5), "cm")) , 
                   align = 'h', 
                   #label_size = 8,
                   labels = "AUTO",
                   hjust = -3, 
                   #vjust= 4,
                   nrow = 2,
                   ncol=1
)

save_plot("graphs/uncertainties.png", uncert.p, dpi=1200, base_width = 210/2, base_height = ((3/5)*210)/2, units="mm", nrow=2, ncol = 1,type = "cairo-png")
save_plot("graphs/uncertainties.pdf", uncert.p, dpi=600, base_width = 210/2, base_height = ((3/5)*210)/2, units="mm",nrow=2, ncol = 1, device = cairo_pdf, fallback_resolution = 1200)
save_plot("graphs/uncertainties.eps", uncert.p, dpi=600, base_width = 210/2, base_height = ((3/5)*210)/2, units="mm",nrow=2, ncol = 1, device = cairo_ps, fallback_resolution = 1200)


