library(devtools)
install_github("hadley/ggplot2")
# (Optional) install_github("ggplot2", "kohske", "feature/new-guides-with-gtable")
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
#library(sf)
options(bitmapType="cairo")
nord <- read_palette("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/nord.ase")

# Increment
abs <- raster("maps/CO2abslEffect_RF_tha.tif")  # Mg/ha
abs.df = as.data.frame(abs,xy=TRUE)

# map the bbox
bbox <- shapefile("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ne_110m_wgs84_bounding_box.VERSION/ne_110m_wgs84_bounding_box.shp") 
bbox_df<- fortify(bbox)
#bbox_robin <- spTransform(bbox, CRS("+proj=eqearth"))  # reproject bounding box
#bbox_robin_df <- fortify(bbox_robin)

# Coastlines
wmap <- readOGR(dsn="~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ne_50m_land", layer="ne_50m_land")
# http://www.naturalearthdata.com/downloads/
wmap_wgs_df <- fortify(wmap)
#wmap_wgs <- spTransform(wmap, CRS("+proj=eqearth"))
#wmap_wgs_df <- fortify(wmap_wgs)

# Ocean
# http://www.naturalearthdata.com/downloads/
ocean <- readOGR(dsn="~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ne_50m_ocean", layer="ne_50m_ocean")
#ocean <- spTransform(ocean, CRS("+proj=eqearth"))
oceanmap_df <- fortify(ocean)

# graticule (Robin)
grat <- shapefile("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ne_110m_graticules_30.VERSION/ne_110m_graticules_30.shp") 
grat_df <- fortify(grat)
#grat_robin <- spTransform(grat, CRS("+proj=eqearth"))  # reproject graticule
#grat_df_robin <- fortify(grat_robin)

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

discrete_gradient_pal <- function(colours, bins = 14) {
  ramp <- scales::colour_ramp(colours)
  
  function(x) {
    if (length(x) == 0) return(character())
    
    i <- floor(x * bins)
    i <- ifelse(i > bins-1, bins-1, i)
    ramp(i/(bins-1))
  }
}

scale_fill_discrete_gradient <- function(..., colours, bins = 14, na.value = "transparent", guide = "colourbar", aesthetics = "fill", colors)  {
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
mycols <- cpt(pal = "arendal_temperature", rev=T, n=15)
mycols <- mycols[2:15]
#mycols[4] <- "#ffffff"
show_col(mycols)

p <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=abs.df,aes(x,y,fill=CO2abslEffect_RF_tha)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    colours = mycols,
    #breaks=-3:11,
    breaks=c(-2,0,2,4,6,8,10),
    limits = c(-3, 11),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab(expression(paste("Change in soil C stocks  (Mg ", ha^-1,")", " in response to ", CO[2] ,sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 12) + 
  theme_opts
p

scale_fill_gradientn(colours = mycols, 
                     #labels = seq(-3, 11, by = 1), 
                     na.value="transparent")
  
scale_fill_gradientn(colours = cpt(pal = "arendal_temperature", rev=T, n=7),
                     values = rescale(c(-3,-1.5,.5,2.5,5,7.5,10)),
                     na.value="transparent")

save_plot("graphs/CO2absEffect.pdf", p, base_aspect_ratio = 1.5, dpi=300)
save_plot("graphs/CO2absEffect.png", p, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

