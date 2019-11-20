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
#library(sf)
options(bitmapType="cairo")
nord <- read_palette("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/nord.ase")

# Increment
abs <- raster("maps/CO2absEffect.tif")  # Mg
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

find_cpt("temperature")
temperature <- cpt(pal = "arendal_temperature", rev=T)
show_col(temperature)
#show_col(viridis_white)

p1 <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=abs.df,aes(x,y,fill=CO2abslEffect_RF_tha)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_gradientn(colours = cpt(pal = "arendal_temperature", rev=T, n=10),
                       values = rescale(c(-1,0,3,5)),
                       na.value="transparent") +
  xlab(expression(paste("Change in soil C stocks (Mg ", ha^-1,")", sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 12) + 
  theme_opts
p1
save_plot("graphs/CO2absEffect.pdf", p1, base_aspect_ratio = 1.5, dpi=300)
save_plot("graphs/CO2absEffect.png", p1, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

display_carto_all() # library(rcartocolor)
my_colors = (carto_pal(4, "Earth"))
hcl_palettes(plot = TRUE) # library(colorspace)
my_colors = diverging_hcl(4, palette = "Blue-Red 3", rev=TRUE)
find_cpt("sunshine_diff_12lev") # library(cptcity)
my_colors = cpt(pal = "ncl_sunshine_diff_12lev",  n=5, rev=T)
show_col(my_colors)

ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  #geom_raster(data=rel.temp.df,aes(x,y,fill=CO2relEffect)) +
  layer_spatial(data=relMgHa_temp) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_gradientn(colours = my_colors,
                       values = rescale(c(-1,-0.5,0,0.1,8)),
                       na.value="transparent") +
  xlab(expression(paste("Change in soil C (Mg ", ha^-1,")", sep=""))) +
  #coord_equal() + 
  coord_sf() +
  theme_classic(base_size = 12) + 
  theme_opts
###################
### ZONAL STATS ###
###################
#esa <- raster("maps/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif")
#esa0p25 <- resample(esa, relMg_temp, method="ngb")
#writeRaster(esa0p25, "maps/ESA_lc_0p25.tif")
esa <- raster("maps/ESA_lc_0p25.tif")
legend <- read.csv("maps/ESA_legend.csv")
s <- stack(esa,relMg_temp, rel_temp)
s.df <- as.data.frame(s)
names(s.df) <- c("ESA", "Mg", "Perc")
s.df <- left_join(s.df,legend, by=c("ESA" = "NB_LAB"))


biome <- s.df %>% dplyr::filter(!is.na(Mg), Mg != 0, !is.na(Perc), Perc != 0) %>% group_by(ESA) %>%  
  dplyr::summarise (MgSum= sum(Mg), PercMean=mean(Perc)) %>% 
  dplyr::mutate(PgSum=round(MgSum*10^-9,digits=2)) %>% droplevels()
sum(biome$PgSum)
write.csv(biome, file = "Summary_ESA.csv")

biome2 <- s.df %>% dplyr::filter(!is.na(Mg), Mg != 0, !is.na(Perc), Perc != 0) %>% group_by(category) %>%  
  dplyr::summarise (MgSum= sum(Mg), PercMean=mean(Perc)) %>% 
  dplyr::mutate(PgSum=round(MgSum*10^-9,digits=2)) %>% droplevels()
sum(biome$PgSum)
write.csv(biome2, file = "Summary_ESAag.csv")

