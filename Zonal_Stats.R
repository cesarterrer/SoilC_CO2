###################
### ZONAL STATS ###
###################
esa <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_2012_aggregated0p25.tif")
legend <- read.csv("ESA_classes.csv")
soil.tpix <- raster("maps/CO2abslEffect_RF_pixel.tif")  # Mg/pixel
soil.tha <- raster("maps/CO2abslEffect_RF_tha.tif")  # Mg/ha
biomass.tpix <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect_TotalBiomass.tif")  # Mg
biomass.a <- area(biomass.tpix)
biomass.tha <- overlay(biomass.tpix, biomass.a, fun=function(x,y) x/(y*100)) # Mg/ha
eco.tpix <- soil.tpix + biomass.tpix
eco.tha <- soil.tha + biomass.tha
soc <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_gm2.tif") # g/m2
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

s2 <- stack(esa,soil.tpix, soil.tha, eco.tpix, eco.tha, soc.tpix, tot.tpix)
s.df <- as.data.frame(s2)
names(s.df) <- c("ESA", "soil.tpix", "soil.tha", "eco.tpix", "eco.tha", "soc.tpix", "tot.tpix")
s.df <- left_join(s.df,legend, by=c("ESA" = "NB_LAB"))
library(magrittr)
biome <- s.df %>% dplyr::filter(!is.na(soil.tpix), soil.tpix != 0) %>% group_by(ESAagg) %>%  
  dplyr::summarise (soil.sum= sum(soil.tpix), soil.area= mean(soil.tha),soc.sum=sum(soc.tpix), 
                    eco.sum=sum(eco.tpix), eco.area= mean(eco.tha), tot.sum=sum(tot.tpix,na.rm=T)) %>% 
  dplyr::mutate(soil.PgC=round(soil.sum*10^-9, digits=2), 
                soil.perc=round((soil.sum*100)/soc.sum, digits=1),
                eco.PgC=round(eco.sum*10^-9, digits=2), 
                eco.perc=round((eco.sum*100)/tot.sum, digits=1)) %>%
  mutate(ESAagg=as.character(ESAagg)) %>% ungroup() %>%
  bind_rows(summarise(.,ESAagg = "Total", soil.sum = sum(soil.sum), soil.area= mean(soil.area), 
                      soc.sum=sum(soc.sum),eco.sum=sum(eco.sum), eco.area= mean(eco.area), tot.sum=sum(tot.sum), 
                      soil.PgC = sum(soil.PgC), eco.PgC = sum(eco.PgC)) %>% 
              mutate(soil.perc=round((soil.sum*100)/soc.sum, digits=1),
                     eco.perc=round((eco.sum*100)/tot.sum, digits=1)))

write.csv(biome, file = "Summary_ESA.csv")


