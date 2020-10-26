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
# Raster layers
biomass <- raster("~/OneDrive - LLNL/IIASA/Upscaling_Biomass/Maps/CO2relEffect.tif")
biomass[is.na(biomass)] <- 0
biomass <- antiperc(biomass)
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
cn <- raster("~/OneDrive - LLNL/IIASA/maps/CNr_arid.tif")
p <- raster("~/OneDrive - LLNL/IIASA/maps/PamountBray1_0_9cm_Resampled.tif")
pts <- read.csv("~/OneDrive - LLNL/FACEreview/Upscaling_ANPP/data/ANPP_effects.csv",na.strings=c("",NA))
pts <- dplyr::filter(pts, !is.na(CNr), !is.na(id), !is.na(PamountBray09_new),Myc=="ECM")
p <- raster::calc(p, fun= function(x) ifelse (x<0,NA,x))
p2 <- focal(p, w=matrix(1,7,7), fun=modal, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
p <- raster::mask(p2,cn)
p <- raster::calc(p, fun= function(x) ifelse (x>max(pts$PamountBray09_new), max(pts$PamountBray09_new), ifelse (x<min(pts$PamountBray09_new), min(pts$PamountBray09_new), x)))

s<- stack(biomass, lai, soc, fert, myc, disturbance, ecotype, tempMean, preSum, cn, p)
names(s) <- c("Effect.biomass","LAImax","Cstock","Nitrogen.fertilization",
              "Symbiotic.type", "Disturbance", "Ecosystem.type", "MAT", "MAP", "CNr", "P")
s.df <- as.data.frame(s,xy=TRUE) %>% rename("Nitrogen.fertilization"=Nitrogen.fertilization_category, "Symbiotic.type" = Symbiotic.type,  
                                            "Disturbance"=Disturbance_category, "Ecosystem.type"=Ecosystem.type_category) %>%
  mutate(Nitrogen.fertilization=factor(Nitrogen.fertilization), 
         Symbiotic.type=recode(Symbiotic.type,"AM", "ECM", "NM"),
         Soil.depth=30,
         Experiment.type=factor("FACE", levels=c("Chamber","FACE","OTC")),
         Disturbance=factor(Disturbance),
         Ecosystem.type=factor(Ecosystem.type))

r.dfNA <- s.df[complete.cases(s.df),]


########## BIOMAS ###########
# Relative
#veg.mod<- readRDS("veg_RF.RData")
veg.mod<- readRDS("mf_cv_veg.RData")
veg.preds <- predict(veg.mod, data = r.dfNA, type="se")
RF.veg <- data.frame(predictions=veg.preds$predictions, se=veg.preds$se)
pred.RF.veg <- cbind(r.dfNA[c("x","y")], RF.veg)
veg.RF <- left_join(s.df,pred.RF.veg)
vegES <- rasterFromXYZ(veg.RF[,c("x", "y", "predictions")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(vegES)
writeRaster(vegES,"maps/Bbiomass.tif",format="GTiff",overwrite=TRUE)

# Absolute
relES <- make_pct(raster("maps/Bbiomass.tif"))
liuras <- brick("~/OneDrive - LLNL/IIASA/maps/Liu_Global_annual_mean_ABC_lc2001_1993_2012_20150331.nc", varname="Aboveground Biomass Carbon") 
liu2012 <- subset(liuras, which(getZ(liuras) ==  2012)) # Subset Year 2012 only; 
liu2012 <-  flip(t(liu2012), direction = "x") # the extent goes beyond the limits (-180, 180, -90, 90). Fix:
extent(liu2012) = c(-180, 180, -90, 90) # Aboveground biomass map is in Mg/ha. I need to compute the total Mg in the world, so I need Mg/pixel:
crs(liu2012) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a <- area(liu2012) # get area of projected raster.
liu_present <- liu2012 * a * 100 # area is in km2 multiply by 100 to get ha 

Bcurrent <- raster::mask(liu_present, relES)
Bfuture <- Bcurrent + Bcurrent * relES/100
Bincrement <- Bfuture - Bcurrent
cellStats(Bincrement,"sum", na.rm=T) * 10^-9 # eCO2-driven Biomass increment
(cellStats(Bincrement,"sum", na.rm=T) * 100) / cellStats(Bcurrent,"sum", na.rm=T) # % increase
writeRaster(Bincrement,"maps/Bbiomass_tpix.tif",format="GTiff",overwrite=TRUE)
a <- area(Bincrement)
inc <- overlay(Bincrement, a, fun=function(x,y) x/(y*100))
writeRaster(inc,"maps/Bbiomass_tha.tif",format="GTiff",overwrite=TRUE)
