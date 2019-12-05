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
esa <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_2012_aggregated0p25.tif")
legend <- read.csv("ESA_classes.csv")
pix <- raster("maps/CO2abslEffect_RF_pixel.tif")  # pixel
gm2 <- raster("maps/CO2abslEffect_RF_gm2.tif")
soc <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_gm2.tif") # g/m2
soc <- mask(soc, pix)
soctha <- soc*0.01 # t(Mg)/ha
soc_a <- area(soctha) # get area of projected raster.
soc_pixel <- soctha * soc_a * 100 # area is in km2 multiply by 100 to get ha 

s <- stack(esa,pix,gm2,soc_pixel)
s.df <- as.data.frame(s)
names(s.df) <- c("ESA", "Mg", "gm2","SOCMg")
s.df <- left_join(s.df,legend, by=c("ESA" = "NB_LAB"))
library(magrittr)
biome <- s.df %>% dplyr::filter(!is.na(Mg), Mg != 0) %>% group_by(ESAagg) %>%  
  dplyr::summarise (MgSum= sum(Mg), Cstock=sum(SOCMg)) %>% 
  dplyr::mutate(PgSum=round(MgSum*10^-9, digits=2), Perc=round((MgSum*100)/Cstock, digits=1)) %>%
  mutate(ESAagg=as.character(ESAagg)) %>% ungroup() %>%
  bind_rows(summarise(.,ESAagg = "Total", MgSum = sum(MgSum), Cstock=sum(Cstock),PgSum = sum(PgSum))%>% 
              mutate(Perc=round((MgSum*100)/Cstock, digits=1)))

write.csv(biome, file = "Summary_ESA.csv")


