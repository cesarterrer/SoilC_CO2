###################
### ZONAL STATS ###
###################
library(raster)
library(tidyverse)
esa <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_2012_aggregated0p25.tif")
legend <- read.csv("ESA_classes.csv")
soil.tpix <- raster("maps/CO2abslEffect_RF_pixel.tif")  # Mg/pixel
soil.tha <- raster("maps/CO2abslEffect_RF_tha.tif")  # Mg/ha
soil.se.tpix <- raster("maps/CO2abslEffect.SE_RF_pixel.tif")  # Mg/pixel
soil.se.tha <- raster("maps/CO2abslEffect.SE_RF_tha.tif")  # Mg/ha
perc <- raster("maps/CO2relEffect_RF_tha.tif")
perc.se <- raster("maps/CO2relEffect.SE_RF_tha.tif")

s2 <- stack(esa, soil.tpix, soil.tha, soil.se.tpix, soil.se.tha, perc, perc.se)
s.df <- as.data.frame(s2)
names(s.df) <- c("ESA", "soil.tpix", "soil.tha", "soil.tpix.se", "soil.tha.se", "soil.perc", "soil.perc.se")
s.df <- left_join(s.df,legend, by=c("ESA" = "NB_LAB"))
library(magrittr)
globalperc <- cellStats(perc,"mean", na.rm=T)
globalperc.se <- cellStats(perc.se,"mean", na.rm=T)

biome <- s.df %>% group_by(ESAagg) %>%  
  dplyr::summarise (soil.sum= sum(soil.tpix,na.rm=T), soil.sum.se=sum(soil.tpix.se,na.rm=T), 
                    soil.area= mean(soil.tha,na.rm=T), soil.area.se= mean(soil.tha.se,na.rm=T), 
                    soil.perc= mean(soil.perc,na.rm=T), soil.perc.se= mean(soil.perc.se,na.rm=T)) %>% 
  dplyr::mutate(soil.area=round(soil.area, digits=1),soil.area.se=round(soil.area.se, digits=1),
                soil.PgC=round(soil.sum*10^-9, digits=1), soil.PgC.se=round(soil.sum.se*10^-9, digits=1),
                soil.perc=round(soil.perc, digits=1),soil.perc.se=round(soil.perc.se, digits=1)) %>%
  mutate(ESAagg=as.character(ESAagg)) %>% ungroup() %>%
  bind_rows(summarise(.,ESAagg = "Total", soil.sum = sum(soil.sum,na.rm=T), soil.sum.se = sum(soil.sum.se,na.rm=T), 
                      soil.area= mean(soil.area,na.rm=T), soil.area.se= mean(soil.area.se,na.rm=T), 
                      soil.PgC = sum(soil.PgC,na.rm=T), soil.PgC.se = sum(soil.PgC.se,na.rm=T),
                      soil.perc= globalperc, soil.perc.se= globalperc.se) %>% 
              mutate(soil.area=round(soil.area, digits=1),soil.area.se=round(soil.area.se, digits=1),
                     soil.PgC=round(soil.sum*10^-9, digits=1), soil.PgC.se=round(soil.sum.se*10^-9, digits=1),
                     soil.perc=round(soil.perc, digits=1),soil.perc.se=round(soil.perc.se, digits=1))) %>%
  mutate(soil.area=paste(soil.area,soil.area.se, sep = " \u00B1 "), 
         soil.PgC=paste(soil.PgC,soil.PgC.se, sep = " \u00B1 "), 
         soil.perc=paste(soil.perc,soil.perc.se, sep = " \u00B1 ")) %>%
  dplyr::select("Biome"=ESAagg,"Soil C (t ha-1)"= soil.area, "Soil C (Pg)"=soil.PgC, "Soil C (%)" = soil.perc) %>%
  filter(!is.na(Biome))

write.csv(biome, file = "Summary_ESA.csv", fileEncoding = 'UTF-8')


######## OLD ########


biomass.tpix <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect_TotalBiomass.tif")  # Mg/pixel
biomass.tha <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect_TotalBiomass_tha.tif")  # Mg/ha
biomass.se.tpix <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absSE_TotalBiomass.tif")  # Mg/pixel
biomass.se.tha <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect_TotalBiomass.SE_tha.tif")  # Mg/ha

eco.tpix <- raster("maps/CO2abslEffect_RF_Ecosystem_tpix.tif")  # Mg/pixel
eco.tha <- raster("maps/CO2abslEffect_RF_Ecosystem_tha.tif")  # Mg/ha
eco.se.tpix <- raster("maps/CO2abslEffect.SE_RF_Ecosystem_tpix.tif")  # Mg/pixel
eco.se.tha <- raster("maps/CO2abslEffect.SE_RF_Ecosystem_tha.tif")  # Mg/ha

soc <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_030cm_aggregated0p25_gm2_zenodo.tif") # g/m2
soc <- mask(soc, soil.tpix)
soc.tha <- soc*0.01 # Mg/ha
soc.a <- area(soc.tha) # get area of projected raster.
soc.tpix <- soc.tha * soc.a * 100 # area is in km2 multiply by 100 to get ha 
abc.tha <- brick("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/Liu_Global_annual_mean_ABC_lc2001_1993_2012_20150331.nc", varname="Aboveground Biomass Carbon") 
abc.tha <- subset(abc.tha, which(getZ(abc.tha) ==  2012)) # Subset Year 2012 only; 
abc.tha <-  flip(t(abc.tha), direction = "x") # the extent goes beyond the limits (-180, 180, -90, 90). Fix:
extent(abc.tha) = c(-180, 180, -90, 90) 
crs(abc.tha) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
abc.a <- area(abc.tha)
abc.tpix <- abc.tha * abc.a * 100
tot.tpix <- soc.tpix + abc.tpix
tot.tha <- soc.tha + abc.tha

s2 <- stack(esa, soil.tpix, soil.tha, soil.se.tpix, soil.se.tha, biomass.tpix, biomass.tha, biomass.se.tpix, biomass.se.tha, 
            eco.tpix, eco.tha, eco.se.tpix, eco.se.tha, soc.tpix, tot.tpix)
s.df <- as.data.frame(s2)
names(s.df) <- c("ESA", "soil.tpix", "soil.tha", "soil.tpix.se", "soil.tha.se", "biomass.tpix", "biomass.tha", "biomass.tpix.se", "biomass.tha.se",
                 "eco.tpix", "eco.tha", "eco.tpix.se", "eco.tha.se", "soc.tpix", "tot.tpix")
s.df <- left_join(s.df,legend, by=c("ESA" = "NB_LAB"))
library(magrittr)
biome <- s.df %>% group_by(ESAagg) %>%  
  dplyr::summarise (soil.sum= sum(soil.tpix,na.rm=T), soil.sum.se=sum(soil.tpix.se,na.rm=T), soil.area= mean(soil.tha,na.rm=T), soil.area.se= mean(soil.tha.se,na.rm=T), 
                    biomass.sum= sum(biomass.tpix,na.rm = T), biomass.sum.se=sum(biomass.tpix.se,na.rm = T), biomass.area= mean(biomass.tha,na.rm = T), biomass.area.se= mean(biomass.tha.se,na.rm = T),
                    eco.sum= sum(eco.tpix,na.rm=T), eco.sum.se=sum(eco.tpix.se,na.rm=T), eco.area= mean(eco.tha,na.rm=T), eco.area.se= mean(eco.tha.se,na.rm=T),
                    soc.sum=sum(soc.tpix,na.rm=T), tot.sum=sum(tot.tpix,na.rm=T)) %>% 
  dplyr::mutate(soil.area=round(soil.area, digits=1),soil.area.se=round(soil.area.se, digits=1),
                biomass.area=round(biomass.area, digits=1),biomass.area.se=round(biomass.area.se, digits=1),
                eco.area=round(eco.area,digits=1),eco.area.se=round(eco.area.se,digits=1),
                soil.PgC=round(soil.sum*10^-9, digits=1), soil.PgC.se=round(soil.sum.se*10^-9, digits=1),
                soil.perc=round((soil.sum*100)/soc.sum, digits=1), soil.perc.se=round((soil.sum.se*100)/soc.sum, digits=1),
                biomass.PgC=round(biomass.sum*10^-9, digits=1), biomass.PgC.se=round(biomass.sum.se*10^-9, digits=1),
                eco.PgC=round(eco.sum*10^-9, digits=1), eco.PgC.se=round(eco.sum.se*10^-9, digits=1),
                eco.perc=round((eco.sum*100)/tot.sum, digits=1), eco.perc.se=round((eco.sum.se*100)/tot.sum, digits=1)) %>%
  mutate(ESAagg=as.character(ESAagg)) %>% ungroup() %>%
  bind_rows(summarise(.,ESAagg = "Total", soil.sum = sum(soil.sum,na.rm=T), soil.sum.se = sum(soil.sum.se,na.rm=T), soil.area= mean(soil.area,na.rm=T), soil.area.se= mean(soil.area.se,na.rm=T),
                      biomass.sum = sum(biomass.sum,na.rm=T), biomass.sum.se = sum(biomass.sum.se,na.rm=T), biomass.area= mean(biomass.area,na.rm=T), biomass.area.se= mean(biomass.area.se,na.rm=T),
                      soc.sum=sum(soc.sum,na.rm=T),eco.sum=sum(eco.sum,na.rm=T), eco.sum.se=sum(eco.sum.se,na.rm=T),eco.area= mean(eco.area,na.rm=T), eco.area.se= mean(eco.area.se,na.rm=T), tot.sum=sum(tot.sum,na.rm=T), 
                      soil.PgC = sum(soil.PgC,na.rm=T), soil.PgC.se = sum(soil.PgC.se,na.rm=T),
                      biomass.PgC = sum(biomass.PgC,na.rm=T),biomass.PgC.se = sum(biomass.PgC.se,na.rm=T),
                      eco.PgC = sum(eco.PgC,na.rm=T),eco.PgC.se = sum(eco.PgC.se,na.rm=T)) %>% 
              mutate(soil.area=round(soil.area,digits=1), soil.area.se=round(soil.area.se,digits=1),
                eco.area=round(eco.area,digits=1), eco.area.se=round(eco.area.se,digits=1), 
                     soil.perc=round((soil.sum*100)/soc.sum, digits=1),soil.perc.se=round((soil.sum.se*100)/soc.sum, digits=1),
                     eco.perc=round((eco.sum*100)/tot.sum, digits=1), eco.perc.se=round((eco.sum.se*100)/tot.sum, digits=1))) %>%
  mutate(soil.area=paste(soil.area,soil.area.se, sep = " \u00B1 "), soil.PgC=paste(soil.PgC,soil.PgC.se, sep = " \u00B1 "), soil.perc=paste(soil.perc,soil.perc.se, sep = " \u00B1 "),
         eco.area=paste(eco.area,eco.area.se, sep = " \u00B1 "), eco.PgC=paste(eco.PgC,eco.PgC.se, sep = " \u00B1 "), eco.perc=paste(eco.perc,eco.perc.se, sep = " \u00B1 ")) %>%
  dplyr::select("Biome"=ESAagg,"Soil C (t ha-1)"= soil.area, "Soil C (Pg)"=soil.PgC, "Soil C (%)" = soil.perc,
                #"Biomass C (t ha-1)"= biomass.area,biomass.area.se, "Biomass C (Pg)"=biomass.PgC,biomass.PgC.se,
                "Ecosystem C (t ha-1)"=eco.area,  "Ecosystem C (Pg)" = eco.PgC, "Ecosystem C (%)" = eco.perc) %>%
  filter(!is.na(Biome))

write.csv(biome, file = "Summary_ESA.csv", fileEncoding = 'UTF-8')
