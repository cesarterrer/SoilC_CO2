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
library(parallel)
library(doParallel)
registerDoParallel(31) # Run in parallel -> 31 cores


# Raster layers
biomass <- raster("maps/Bbiomass.tif")
biomass[is.na(biomass)] <- 0
lai <- raster("~/OneDrive - LLNL/IIASA/maps/LAImax_2012_aggregated0p25.tif")
#soc <- raster("~/OneDrive - LLNL/IIASA/maps/SOC_015cm_aggregated0p25_gm2.tif")
soc <- raster("~/OneDrive - LLNL/IIASA/maps/SOC_015cm_aggregated0p25_tha_SoilGrids2.tif") *100
soc <- projectRaster(soc,biomass)
fert <- raster("~/OneDrive - LLNL/IIASA/maps/ESA_Nfertilization.tif")
disturbance <- raster("~/OneDrive - LLNL/IIASA/maps/ESA_Disturbance.tif")
ecotype <- raster("~/OneDrive - LLNL/IIASA/maps/ESA_EcosystemType.tif")
temp <- cruts2raster("~/OneDrive - LLNL/IIASA/maps/cru_ts4.00.2011.2015.tmp.dat.nc", timeRange=c("2012-01-01","2012-12-31")) # 2012; 
tempMean <- mean (temp) # Average temperature
tempMean <- resample(tempMean,biomass)
prec <- cruts2raster("~/OneDrive - LLNL/IIASA/maps/cru_ts4.00.2011.2015.pre.dat.nc", timeRange=c("2012-01-01","2012-12-31")) # 2012; 
preSum <- sum (prec) # Sum precipitation
preSum <- resample(preSum,biomass)
myc <- raster("maps/Myco_AMEMeco.tif")

s<- stack(biomass, lai, soc, fert, myc, disturbance, ecotype, tempMean, preSum)
names(s) <- c("Effect.biomass","LAImax","Cstock","Nitrogen.fertilization",
              "Symbiotic.type", "Disturbance", "Ecosystem.type", "MAT", "MAP")
s.df <- as.data.frame(s,xy=TRUE) %>% rename("Nitrogen.fertilization"=Nitrogen.fertilization_category, "Symbiotic.type" = Symbiotic.type,  
                                            "Disturbance"=Disturbance_category, "Ecosystem.type"=Ecosystem.type_category) %>%
  mutate(Nitrogen.fertilization=factor(Nitrogen.fertilization), 
         Symbiotic.type=recode(Symbiotic.type,"AM", "ECM", "NM"),
         Soil.depth=30,
         Experiment.type=factor("FACE", levels=c("Chamber","FACE","OTC")),
         Disturbance=factor(Disturbance),
         Ecosystem.type=factor(Ecosystem.type))

r.dfNA <- s.df[complete.cases(s.df),]

#################### META FOREST #################

## SOC ABSOLUTE ##
mod <- readRDS("mf_cv_abs.RData")
forest.abs <- mod$finalModel
number_of_chunks = 200
library(parallel)
library(doParallel)
registerDoParallel(31) # Run in parallel -> 31 cores
preds <- lapply(seq(1, NROW(r.dfNA), ceiling(NROW(r.dfNA)/number_of_chunks)),
                  function(i) {
                    df_tmp <- r.dfNA[i:min(i + ceiling(NROW(r.dfNA)/number_of_chunks) - 1, NROW(r.dfNA)),]
                    predict(forest.abs, data = df_tmp, type="se")
                  })
RF <- data.frame(predictions=unlist(lapply(preds, `[[`, "predictions")), se=unlist(lapply(preds, `[[`, "se")) )
pred.RF <- cbind(r.dfNA[c("x","y")], RF)
abs.preds <- left_join(s.df,pred.RF)
absES <- rasterFromXYZ(abs.preds[,c("x", "y", "predictions")],crs="+proj=longlat +datum=WGS84")
plot(absES)
absSE <- rasterFromXYZ(abs.preds[,c("x", "y", "se")],crs="+proj=longlat +datum=WGS84")
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
forest.rel <- readRDS("reduced_rel.RData")
## predict ##
number_of_chunks = 200
preds.rel <- lapply(seq(1, NROW(r.dfNA), ceiling(NROW(r.dfNA)/number_of_chunks)),
                function(i) {
                  df_tmp <- r.dfNA[i:min(i + ceiling(NROW(r.dfNA)/number_of_chunks) - 1, NROW(r.dfNA)),]
                  predict(forest.rel, data = df_tmp, type="se")
                })
RF.rel <- data.frame(predictions=unlist(lapply(preds.rel, `[[`, "predictions")), se=unlist(lapply(preds.rel, `[[`, "se")) )
pred.RF.rel <- cbind(r.dfNA[c("x","y")], RF.rel)
rel <- left_join(s.df,pred.RF.rel)
relES <- make_pct(rasterFromXYZ(rel[,c("x", "y", "predictions")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(relES)
relSE <- make_pct(rasterFromXYZ(rel[,c("x", "y", "se")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(relSE)
cellStats(relES,"mean", na.rm=T) 
cellStats(relSE,"mean", na.rm=T) 
writeRaster(relES,"maps/CO2relEffect_RF_tha.tif",format="GTiff",overwrite=TRUE)
writeRaster(relSE,"maps/CO2relEffect.SE_RF_tha.tif",format="GTiff",overwrite=TRUE)




