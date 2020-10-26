###################
### ZONAL STATS ###
###################
library(raster)
library(tidyverse)
esa <- raster("~/OneDrive - LLNL/IIASA/maps/ESA_2012_aggregated0p25.tif")
legend <- read.csv("ESA_classes.csv")
soil.tpix <- raster("maps/CO2abslEffect_RF_pixel.tif")  # Mg/pixel
soil.tha <- raster("maps/CO2abslEffect_RF_tha.tif") *100  # g/m2
soil.se.tpix <- raster("maps/CO2abslEffect.SE_RF_pixel.tif")  # Mg/pixel
soil.se.tha <- raster("maps/CO2abslEffect.SE_RF_tha.tif")  *100  # g/m2
perc <- raster("maps/CO2relEffect_RF_tha.tif")
perc.se <- raster("maps/CO2relEffect.SE_RF_tha.tif")

s2 <- stack(esa, soil.tpix, soil.tha, soil.se.tpix, soil.se.tha, perc, perc.se)
r<-init(s2,"y")
rc <- reclassify(r, c(-90,-15,NA, -15,15,1, 15,60,NA,60,90,1))
s2 <- mask(s2,rc,maskvalue=1)

s.df <- as.data.frame(s2)
names(s.df) <- c("ESA", "soil.tpix", "soil.tha", "soil.tpix.se", "soil.tha.se", "soil.perc", "soil.perc.se")
s.df <- left_join(s.df,legend, by=c("ESA" = "NB_LAB"))
library(magrittr)
globalperc <- cellStats(perc,"mean", na.rm=T)
globalperc.se <- cellStats(perc.se,"mean", na.rm=T)
globalarea <- cellStats(soil.tha,"mean", na.rm=T)
globalarea.se <- cellStats(soil.se.tha,"mean", na.rm=T)

biome <- s.df %>% group_by(ESAagg) %>%  
  dplyr::summarise (soil.sum= sum(soil.tpix,na.rm=T), soil.sum.se=sum(soil.tpix.se,na.rm=T), 
                    soil.area= mean(soil.tha,na.rm=T), soil.area.se= mean(soil.tha.se,na.rm=T), 
                    soil.perc= mean(soil.perc,na.rm=T), soil.perc.se= mean(soil.perc.se,na.rm=T)) %>% 
  dplyr::mutate(soil.area=round(soil.area, digits=1),soil.area.se=round(soil.area.se, digits=1),
                soil.PgC=round(soil.sum*10^-9, digits=1), soil.PgC.se=round(soil.sum.se*10^-9, digits=1),
                soil.perc=round(soil.perc, digits=1),soil.perc.se=round(soil.perc.se, digits=1)) %>%
  mutate(ESAagg=as.character(ESAagg)) %>% ungroup() %>%
  bind_rows(summarise(.,ESAagg = "Total", soil.sum = sum(soil.sum,na.rm=T), soil.sum.se = sum(soil.sum.se,na.rm=T), 
                      soil.area= globalarea, soil.area.se= globalarea.se, 
                      soil.PgC = sum(soil.PgC,na.rm=T), soil.PgC.se = sum(soil.PgC.se,na.rm=T),
                      soil.perc= globalperc, soil.perc.se= globalperc.se) %>% 
              mutate(soil.area=round(soil.area, digits=1),soil.area.se=round(soil.area.se, digits=1),
                     soil.PgC=round(soil.sum*10^-9, digits=1), soil.PgC.se=round(soil.sum.se*10^-9, digits=1),
                     soil.perc=round(soil.perc, digits=1),soil.perc.se=round(soil.perc.se, digits=1))) %>%
  mutate(soil.area=paste(soil.area,soil.area.se, sep = " \u00B1 "), 
         soil.PgC=paste(soil.PgC,soil.PgC.se, sep = " \u00B1 "), 
         soil.perc=paste(soil.perc,soil.perc.se, sep = " \u00B1 ")) %>%
  dplyr::select("Biome"=ESAagg,"Soil C (g m-2)"= soil.area, "Soil C (Pg)"=soil.PgC, "Soil C (%)" = soil.perc) %>%
  filter(!is.na(Biome))

write.csv(biome, file = "Summary_ESA.csv", fileEncoding = 'UTF-8')
