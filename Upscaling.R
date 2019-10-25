library(devtools)
install_github("wviechtb/metafor")
library(dplyr)
library(metafor)
library(raster)

make_pct <- function(x) (exp(x) - 1) * 100
antiperc <- function(x) log(x/100 + 1)
#####################
dat <- read.csv("soilC_meta.csv")
dat <- filter(dat, biomass != "NA")
dat <- filter(dat, nyears >= 0.5)
dat <- filter(dat, N=="Nlow", Experiment_type != "Chamber", Disturbance=="intact", Myc != "Nfixer")

dat$ECM <- with(dat, ifelse(Myc=="ECM", 100,0))

####### META-MODEL ######
final_aicc <- rma(yi, vi, data=dat, mods= ~ ECM + biomass,
                  knha=TRUE, control=list(stepadj=0.5))
summary(final_aicc)

####### LAYERS #######
ecm <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrEM_current_all.tif")
er <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrER_current_all.tif")
am <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrAM_current_all.tif")
nm <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrNM_current_all.tif")
ecm_er <- ecm+er # SUM ECM and Ericoid
biomass <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2relEffect.tif")
biomass <- antiperc(biomass)
ecm.new <- resample(ecm_er,biomass,method="bilinear")
nm.new <- resample(nm,biomass,method="bilinear")
# Soil organic carbon stock in kg/m2 at 0–10cm at 250 m resolution by SoilGrids team stored at LandGIS
# https://zenodo.org/record/2536040#.XW33XC2Q1QI
#soc <- raster("maps/sol_organic.carbon.stock_msa.kgm2_m_250m_b0..10cm_1950..2017_v0.2.tif")
#soc.new <- resample(soc,biomass,method="bilinear")
#writeRaster(soc.new,"maps/sol_organic.carbon.stock_025degrees.tif",overwrite=TRUE)
soc <- raster("maps/sol_organic.carbon.stock_025degrees.tif") # kg/m2
soc.new <- soc*10 # Mg/ha == tonnes/ha. I need to compute the total Mg in the world, so I need Mg/pixel:
a <- area(soc.new) # get area of projected raster.
soc_present <- soc.new * a * 100 # area is in km2 multiply by 100 to get ha 
cellStats(soc_present,"sum", na.rm=T) * 10^(-9)   # Pg soil C at 0-10cm
cellStats(soc_present,"mean", na.rm=T)  # Average size (Mg) soil C pool per pixel at 0-10cm

######## RELATIVE EFFECT ###########
s<- stack(ecm.new, biomass)
names(s)<- c("ECM","biomass")
s.df <- as.data.frame(s,xy=TRUE)

options(na.action = "na.pass")
pred <- as.data.frame.list(predict(final_aicc, newmods =cbind(s.df$ECM, s.df$biomass), addx=T))
pred <- cbind(s.df[1:2], pred)
relES <- rasterFromXYZ(pred[,c("x", "y", "pred")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
relES <- make_pct(relES)
# Add-ons
# 1. Correct in areas with no vegetation using the map of non-mycorrhizal
relES <- relES - relES*nm.new/100
# 2. Areas with biomass==0 were in reality NA due to missing P data
relES <- mask(relES, biomass, maskvalue=0,updatevalue=0)
writeRaster(relES,"maps/CO2relEffect.tif",overwrite=TRUE)

######### ABSOLUTE EFFECT #########
Ccurrent <- raster::mask(soc_present, relES)
Cfuture <- Ccurrent + Ccurrent * relES/100
Cincrement <- Cfuture - Ccurrent
cellStats(Cincrement,"sum", na.rm=T) * 10^-9 # eCO2-driven soil C increment
(cellStats(Cincrement,"sum", na.rm=T) * 100) / cellStats(Ccurrent,"sum", na.rm=T) # % increase
writeRaster(Cincrement,"maps/CO2absEffect.tif",overwrite=TRUE)
