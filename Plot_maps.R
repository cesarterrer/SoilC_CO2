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

# Increment
abs <- raster("maps/CO2abslEffect_RF_tha.tif")  # Mg/ha
abs <- projectRaster(abs,crs="+proj=robin",over=T)
abs.df = as.data.frame(abs,xy=TRUE)
range(abs.df$CO2abslEffect_RF_tha, na.rm=T)

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
mycols <- cpt(pal = "arendal_temperature", rev=T, n=13)
mycols <- mycols[2:12]
#mycols[4] <- "#ffffff"
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
  xlab(expression(paste("Change in soil C stocks  (Mg ", ha^-1,")", " in response to ", CO[2] ,sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 10) + 
  theme_opts
p

save_plot("graphs/CO2absEffect.pdf", p, base_aspect_ratio = 1.5, dpi=300)
save_plot("graphs/CO2absEffect.png", p, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

##### BIOMASS #####
biomass <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect.tif")  # Mg
biomass[is.na(biomass)] <- 0
biomass.se <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absSE.tif")
biomass.se[is.na(biomass.se)] <- 0
esa.pan <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_PANratios.tif")
total.s <- stack(esa.pan,biomass,biomass.se)
total.df <- as.data.frame(total.s,xy=TRUE)
#Calculate the increase in TOTAL BIOMASS using TB ratios from Liu et al.
library("readxl")
ratios <- read_excel("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/TBratio.xlsx", col_types = c("text", "numeric", "numeric", "guess", "guess"))
total.df <- merge(total.df,ratios, "ESA_PANratios_category")
total.df$TotBiom <- round(total.df$CO2absEffect * total.df$TBratio,2)
total.df$TotBiomSE <- round(total.df$CO2absSE * total.df$TBratio,2)
total.biomass <- rasterFromXYZ(total.df[,c("x", "y", "TotBiom")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
total.biomass.se <- rasterFromXYZ(total.df[,c("x", "y", "TotBiomSE")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
writeRaster(total.biomass,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect_TotalBiomass.tif",overwrite=TRUE)
writeRaster(total.biomass.se,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absSE_TotalBiomass.tif",overwrite=TRUE)

total.biomass.a <- area(total.biomass)
total.biomasstha <- overlay(total.biomass, total.biomass.a, fun=function(x,y) x/(y*100)) # Mg/ha
crs(total.biomasstha) <- "+proj=longlat"
total.biomasstha<- projectRaster(total.biomasstha,crs="+proj=eqearth",over=T)
biomass.df = as.data.frame(total.biomasstha,xy=TRUE)

##### ECOSYSTEM C ######
eco <- abs + total.biomasstha
eco.df = as.data.frame(eco,xy=TRUE)
range(eco.df$layer,na.rm=T)

my_viridis <- c("#FFFFFF", viridis(n=9, direction=-1))
show_col(my_viridis)

p2 <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=eco.df,aes(x,y,fill=layer)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    bins = 10,
    colours = my_viridis,
    #breaks=-3:11,
    breaks=c(0,10,20,30,40,50),
    limits = c(0, 50),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab(expression(paste("Change in ecosystem C stocks  (Mg ", ha^-1,")", " in response to ", CO[2] ,sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 10) + 
  theme_opts
p2

save_plot("graphs/CO2absEffect_Ecosystem.pdf", p2, base_aspect_ratio = 1.5, dpi=300)
save_plot("graphs/CO2absEffect_Ecosystem.png", p2, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

# FIGURE 3
fig3 <- plot_grid( p + theme(plot.margin = unit(c(0,-.5,-1,-.5), "cm")) + xlab(expression(paste(eCO[2], " effect on soil C stocks (Mg ", ha^-1,")" ,sep=""))), 
                  p2 + theme(plot.margin = unit(c(-1,-.5,0,-.5), "cm")) + xlab(expression(paste(eCO[2], " effect on ecosystem C stocks (Mg ", ha^-1,")" ,sep=""))), 
                                 align = 'h', 
                                 #label_size = 8,
                                 labels = "auto",
                                 hjust = -3, 
                                 #vjust= 4,
                                 nrow = 2,
                                 ncol=1
)

save_plot("graphs/Fig3.png", fig3, dpi=1200, base_width = 210/2, base_height = ((3/5)*210)/2, units="mm", nrow=2, ncol = 1,type = "cairo-png")
save_plot("graphs/Fig3.pdf", fig3, dpi=600, base_width = 210/2, base_height = ((3/5)*210)/2, units="mm",nrow=2, ncol = 1, device = cairo_pdf, fallback_resolution = 1200)
save_plot("graphs/Fig3.eps", fig3, dpi=600, base_width = 210/2, base_height = ((3/5)*210)/2, units="mm",nrow=2, ncol = 1, device = cairo_ps, fallback_resolution = 1200)



