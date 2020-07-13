library(devtools)
#devtools::install_github("imbs-hl/ranger")
devtools::install_github("cjvanlissa/metaforest")
#library(ranger)
library(metaforest)
library(tidyverse)
library(metafor)
library(raster)
library(caret)
library(cruts)
library(ncdf4)
make_pct <- function(x) (exp(x) - 1) * 100
antiperc <- function(x) log(x/100 + 1)

#################### META FOREST #################

non.fert.mod <- readRDS("non_fert_mod.RData")
fert.mod <- readRDS("fert_mod.RData")

# Raster layers
biomass <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2relEffect.tif")
biomass[is.na(biomass)] <- 0
biomass <- antiperc(biomass)
lai <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/LAImax_2012_aggregated0p25.tif")
#soc <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_gm2.tif")
soc <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_tha_SoilGrids2.tif") *100
soc <- projectRaster(soc,biomass)
fert <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_Nfertilization.tif")
disturbance <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_Disturbance.tif")
ecotype <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_EcosystemType.tif")
temp <- cruts2raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/cru_ts4.00.2011.2015.tmp.dat.nc", timeRange=c("2012-01-01","2012-12-31")) # 2012; 
tempMean <- mean (temp) # Average temperature
tempMean <- resample(tempMean,biomass)
prec <- cruts2raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/cru_ts4.00.2011.2015.pre.dat.nc", timeRange=c("2012-01-01","2012-12-31")) # 2012; 
preSum <- sum (prec) # Sum precipitation
preSum <- resample(preSum,biomass)
myc <- raster("maps/Myco_AMEM.tif")
s<- stack(biomass, lai, soc, fert, myc, disturbance, ecotype, tempMean, preSum)
names(s) <- c("Effect.biomass","LAImax","Cstock","Nitrogen.fertilization",
              "Symbiotic.type", "Disturbance", "Ecosystem.type", "MAT", "MAP")
s.df <- as.data.frame(s,xy=TRUE) %>% rename("Nitrogen.fertilization"=Nitrogen.fertilization_category, "Symbiotic.type" = Symbiotic.type_category,  
                                            "Disturbance"=Disturbance_category, "Ecosystem.type"=Ecosystem.type_category) %>%
  mutate(Nitrogen.fertilization=factor(Nitrogen.fertilization), 
         Symbiotic.type=factor(Symbiotic.type),
         Soil.depth=30,
         Experiment.type=factor("FACE", levels=c("Chamber","FACE","OTC")),
         Disturbance=factor(Disturbance),
         Ecosystem.type=factor(Ecosystem.type))

r.dfNA <- s.df[complete.cases(s.df),]
nonfert.df <- dplyr::filter(r.dfNA,Nitrogen.fertilization=="Nlow")
fert.df <- dplyr::filter(r.dfNA,Nitrogen.fertilization=="Nhigh")
## PREDICT ##
number_of_chunks = 200
nonfert.pred <- lapply(seq(1, NROW(nonfert.df), ceiling(NROW(nonfert.df)/number_of_chunks)),
                  function(i) {
                    df_tmp <- nonfert.df[i:min(i + ceiling(NROW(nonfert.df)/number_of_chunks) - 1, NROW(nonfert.df)),]
                    predict(non.fert.mod, data = df_tmp, type="se")
                  })
fert.pred <- lapply(seq(1, NROW(fert.df), ceiling(NROW(fert.df)/number_of_chunks)),
                       function(i) {
                         df_tmp <- fert.df[i:min(i + ceiling(NROW(fert.df)/number_of_chunks) - 1, NROW(fert.df)),]
                         predict(fert.mod, data = df_tmp, type="se")
                       })

nonfertRF <- data.frame(predictions=unlist(lapply(nonfert.pred, `[[`, "predictions")), se=unlist(lapply(nonfert.pred, `[[`, "se")) )
fertRF <- data.frame(predictions=unlist(lapply(fert.pred, `[[`, "predictions")), se=unlist(lapply(fert.pred, `[[`, "se")) )

nonfert <- cbind(nonfert.df[c("x","y")], nonfertRF)
fert <- cbind(fert.df[c("x","y")], fertRF)
preds <- bind_rows(nonfert,fert)
abs <- left_join(s.df,preds)
absES <- rasterFromXYZ(abs[,c("x", "y", "predictions")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(absES)
absSE <- rasterFromXYZ(abs[,c("x", "y", "se")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(absSE)
cellStats(absES,"mean", na.rm=T) # g/m2
cellStats(absSE,"mean", na.rm=T) # g/m2
writeRaster(absES,"maps/CO2abslEffect_RF_gm2.tif",format="GTiff",overwrite=TRUE)
writeRaster(absSE,"maps/CO2abslEffect.SE_RF_gm2.tif",format="GTiff",overwrite=TRUE)
absEStha <- absES*0.01 # Mg/ha == tonnes/ha. I need to compute the total Mg in the world, so I need Mg/pixel:
absSEtha <- absSE*0.01 # Mg/ha == tonnes/ha. I need to compute the total Mg in the world, so I need Mg/pixel:
writeRaster(absEStha,"maps/CO2abslEffect_RF_tha.tif",format="GTiff",overwrite=TRUE)
writeRaster(absSEtha,"maps/CO2abslEffect.SE_RF_tha.tif",format="GTiff",overwrite=TRUE)
a <- area(absEStha) # get area of projected raster.
absESpixel <- absEStha * a * 100 # area is in km2 multiply by 100 to get ha 
absSEpixel <- absSEtha * a * 100 # area is in km2 multiply by 100 to get ha 
cellStats(absESpixel,"sum", na.rm=T) * 10^(-9)   # Pg soil C at 0-30cm
cellStats(absSEpixel,"sum", na.rm=T) * 10^(-9)   # Pg soil C at 0-30cm
writeRaster(absESpixel,"maps/CO2abslEffect_RF_pixel.tif",format="GTiff",overwrite=TRUE)
writeRaster(absSEpixel,"maps/CO2abslEffect.SE_RF_pixel.tif",format="GTiff",overwrite=TRUE)

### PERCENTAGE ###
soc <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_030cm_aggregated0p25_gm2_zenodo.tif") # g/m2
soc.tha <- soc*0.01 # Mg/ha
#soc.tha <- raster("soc_0-30cm_0p25_SoilGrids2.tif") # t/ha
#soc.tha <- resample(soc.tha,absEStha)
#soc.tha <- mask(soc.tha, absEStha)
perc <- (absEStha*100)/soc.tha
percSE <- (absSEtha*100)/soc.tha
esa <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_2012_aggregated0p25.tif")
legend <- read.csv("ESA_classes.csv")
soc.eco <- stack(esa, perc, percSE)
soc.rel.df <- as.data.frame(soc.eco,xy=TRUE)
names(soc.rel.df) <- c("x","y","ESA", "perc", "perc.se")
soc.rel.df <- left_join(soc.rel.df,legend, by=c("ESA" = "NB_LAB")) 
soc.rel.out.df <- soc.rel.df %>% group_by(ESAagg) %>%
  mutate(perc2=ifelse(perc>quantile(perc,.90,na.rm=T),quantile(perc,.90,na.rm=T),perc),
         se2=ifelse(perc.se>quantile(perc.se,.90,na.rm=T),quantile(perc.se,.90,na.rm=T),perc.se))
perc.final <- rasterFromXYZ(soc.rel.out.df[,c("x", "y", "perc2")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
perc.final.se <- rasterFromXYZ(soc.rel.out.df[,c("x", "y", "se2")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
cellStats(perc.final,"mean", na.rm=T)
cellStats(perc.final.se,"mean", na.rm=T)

writeRaster(perc.final,"maps/CO2relEffect_RF_tha.tif",format="GTiff",overwrite=TRUE)
writeRaster(perc.final.se,"maps/CO2relEffect.SE_RF_tha.tif",format="GTiff",overwrite=TRUE)

##### ECOSYSTEM C ######
plus <- function(x) {
  if(all(is.na(x))){
    c(x[0],NA)} else {
      sum(x,na.rm = TRUE)}
}
plus.se <- function(x) {
  if(all(is.na(x))){
    c(x[0],NA)} else {
      sqrt(sum(x^2,na.rm = TRUE))}
}
total.biomasstha <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect_TotalBiomass_tha.tif")
total.biomasstha.se<- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect_TotalBiomass.SE_tha.tif")
total.biomasstpix <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect_TotalBiomass.tif")
total.biomasstpix.se <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absSE_TotalBiomass.tif")

eco<- stack(absEStha,total.biomasstha)
eco.sum <- calc(eco,plus)
writeRaster(eco.sum,"maps/CO2abslEffect_RF_Ecosystem_tha.tif",format="GTiff",overwrite=TRUE)
eco.pix<- stack(absESpixel,total.biomasstpix)
eco.pix.sum <- calc(eco.pix, plus)
writeRaster(eco.pix.sum,"maps/CO2abslEffect_RF_Ecosystem_tpix.tif",format="GTiff",overwrite=TRUE)
cellStats(eco.pix.sum,"sum", na.rm=T) * 10^(-9)

eco.se<- stack(absSEtha,total.biomasstha.se)
eco.se.sum <- calc(eco.se,plus.se)
writeRaster(eco.se.sum,"maps/CO2abslEffect.SE_RF_Ecosystem_tha.tif",format="GTiff",overwrite=TRUE)
eco.se.pix<- stack(absSEpixel,total.biomasstpix.se)
eco.se.pix.sum <- calc(eco.se.pix, plus.se)
writeRaster(eco.se.pix.sum,"maps/CO2abslEffect.SE_RF_Ecosystem_tpix.tif",format="GTiff",overwrite=TRUE)
cellStats(eco.se.pix.sum,"sum", na.rm=T) * 10^(-9)
