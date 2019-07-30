library(raster)
library(dplyr)
library(rworldmap)
library(ggplot2)
library(cowplot)
library(cruts)
library(ncdf4)
library(ggbiome)
library(rgdal)
library(maps)
library(ggmap)
library(ggalt)
library(viridis)
require(Cairo)
library(plotbiomes)
library(rcartocolor)

theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         #plot.background = element_rect(fill="#e6e8ed"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title = element_blank(),
                         plot.title = element_text(size=22)))


effects <- read.csv("soilC_meta.csv")
effects <- filter(effects, biomass != "NA")
effects <- filter(effects, nyears >= 0.5)


### Locations ###
effects$Latitude <- ifelse(effects$Lat=="S",abs(effects$Latitude) * -1,abs(effects$Latitude))
effects$Longitude <- ifelse(effects$Lon=="W",abs(effects$Longitude) * -1,abs(effects$Longitude))

world <- map_data("world",wrap=c(-170,180, NA)) %>% filter(region != "Antarctica")
#levels(effects$Biome) <- list("Boreal forest"= "Boreal_Forest", "Cropland"="Cropland", "Grassland"="Grassland",
 #                             "Shrubland"="Shrubland", "Temperate forest"="Temperate_Forest","Tropical forest" = "Tropical_Forest")

display_carto_all()
my_bold <- carto_pal(12, "Bold")
library(scales)
show_col(my_bold)
my_bold <- carto_pal(12, "Bold")[c(1,4,5,2,3)]

studies <- ggplot() + 
  geom_polygon(data=world, aes(long, lat, group = paste(region, group)), fill="grey") + 
  coord_fixed() +
  geom_point(data = effects, aes(x = jitter(Longitude,factor=100), y = jitter(Latitude,factor=100), fill= Ecosystem.type),
             alpha=0.6, colour="black",position = "jitter",
             shape=21, size=4) +
  scale_fill_manual(values = my_bold)+
  coord_proj(paste0("+proj=robin")) +
  theme_cowplot(font_size = 12) +
  #geom_text_repel(data=effects, size = 1, aes(y=Latitude, x=Longitude,label=Experiment)) +
  theme_opts + theme(legend.title=element_text(size=11),
                     legend.position= c(0,0.5),
                     legend.text.align = 0,
                     legend.spacing.y = unit(0, "cm")) +
  guides (fill=guide_legend(title=NULL, override.aes=list(shape=22, alpha=1,size=3)))
studies

library(Cairo)
options(bitmapType="cairo")
save_plot("graphs/locations.pdf", studies, base_height = 4, base_width = 9, device = cairo_pdf, fallback_resolution = 1200)
save_plot("graphs/locations.png", studies, base_height = 4, base_width = 9, dpi=800,type = "cairo-png")
save_plot("graphs/locations.eps", studies, base_height = 4, base_width = 9, device=cairo_ps, fallback_resolution = 1200)
