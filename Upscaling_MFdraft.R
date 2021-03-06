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
#mf_cv_abs <- readRDS("mf_cv_abs.RData")
#forest.abs <- mf_cv_abs$finalModel
#VarImpPlot(forest.abs)
forest.abs <- readRDS("full_reduced_mod.RData")

# Raster layers
biomass <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2relEffect.tif")
biomass[is.na(biomass)] <- 0
antiperc <- function(x) log(x/100 + 1)
biomass <- antiperc(biomass)
lai <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/LAImax_2012_aggregated0p25.tif")
fpar <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/FPARmax_2012_aggregated0p25.tif")
#soc <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_gm2.tif")
soc <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_tha_SoilGrids2.tif") *100
soc <- projectRaster(soc,biomass)
fert <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_Nfertilization.tif")
disturbance <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_Disturbance.tif")
ecotype <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_EcosystemType.tif")
ecm <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrEM_current_all.tif")
#er <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrER_current_all.tif")
#am <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrAM_current_all.tif")
#nm <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrNM_current_all.tif")
temp <- cruts2raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/cru_ts4.00.2011.2015.tmp.dat.nc", timeRange=c("2012-01-01","2012-12-31")) # 2012; 
tempMean <- mean (temp) # Average temperature
tempMean <- resample(tempMean,biomass)
prec <- cruts2raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/cru_ts4.00.2011.2015.pre.dat.nc", timeRange=c("2012-01-01","2012-12-31")) # 2012; 
preSum <- sum (prec) # Sum precipitation
preSum <- resample(preSum,biomass)
#Myc <- stack(am,ecm,er,nm)
#Myc.df <-as.data.frame(Myc,xy=TRUE) %>% rename(AM=MycDistrAM_current_all,ECM=MycDistrEM_current_all, 
   #                                            ER=MycDistrER_current_all, NM=MycDistrNM_current_all)
#Myc.df <- Myc.df %>% rownames_to_column('id') %>%
 # left_join(Myc.df %>% rownames_to_column('id') %>%
  #    pivot_longer(names_to="max_myc", values_to="max_cnt", cols=AM:NM) %>% 
   #   group_by(id) %>% 
    #  slice(which.max(max_cnt))) 
#Myc.df$max_myc <- factor(Myc.df$max_myc)
#lev <- levels(Myc.df$max_myc)
#Myc.df$max_myc <- as.numeric(Myc.df$max_myc)
#myc <- rasterFromXYZ(Myc.df[,c("x", "y", "max_myc")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#myc <- resample(myc,biomass,method="ngb")
#levels(myc) <- data.frame(ID = 1:length(lev), code = lev)
#writeRaster(myc,"maps/Myco_AMEM.tif",format="GTiff",overwrite=TRUE)
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

## PREDICT ##
number_of_chunks = 200
pred_se <- lapply(seq(1, NROW(r.dfNA), ceiling(NROW(r.dfNA)/number_of_chunks)),
       function(i) {
         df_tmp <- r.dfNA[i:min(i + ceiling(NROW(r.dfNA)/number_of_chunks) - 1, NROW(r.dfNA)),]
         predict(forest.abs, data = df_tmp, type="se")
         })
RFpred <- unlist(lapply(pred_se, `[[`, "predictions"))
RFpredSE <- unlist(lapply(pred_se, `[[`, "se"))
absES <- cbind(r.dfNA[c("x","y")], RFpred)
absES <- left_join(s.df,absES)
absES <- rasterFromXYZ(absES[,c("x", "y", "RFpred")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(absES)
absSE <- cbind(r.dfNA[c("x","y")], RFpredSE)
absSE <- left_join(s.df,absSE)
absSE <- rasterFromXYZ(absSE[,c("x", "y", "RFpredSE")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
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
#soc <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_030cm_aggregated0p25_gm2_zenodo.tif") # g/m2
soc.tha <- raster("soc_0-30cm_0p25_SoilGrids2.tif") # t/ha
soc.tha <- resample(soc.tha,absEStha)
#soc.tha <- soc*0.01 # Mg/ha
soc.tha <- mask(soc.tha, absEStha)
perc <- (absEStha*100)/soc.tha
perc <- raster::calc(perc, fun= function(x) ifelse (x>quantile(perc,.95),quantile(perc,.95),x)) 
cellStats(perc,"mean", na.rm=T)
writeRaster(perc,"maps/CO2relEffect_RF_tha.tif",format="GTiff",overwrite=TRUE)
percSE <- (absSEtha*100)/soc.tha
percSE <- raster::calc(percSE, fun= function(x) ifelse (x>quantile(percSE,.95),quantile(percSE,.95),x))
cellStats(percSE,"mean", na.rm=T)
writeRaster(percSE,"maps/CO2relEffect.SE_RF_tha.tif",format="GTiff",overwrite=TRUE)

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
