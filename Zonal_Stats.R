



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

s2 <- stack(esa,pix,gm2,soc_pixel)
s.df <- as.data.frame(s2)
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


