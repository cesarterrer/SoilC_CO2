library(devtools)
install_github("cjvanlissa/metaforest")
library(metaforest)
library(dplyr)
library(metafor)
library(raster)

make_pct <- function(x) (exp(x) - 1) * 100
antiperc <- function(x) log(x/100 + 1)
#####################
dat <- read.csv("soilC_meta.csv")
dat <- filter(dat, biomass != "NA")
dat <- filter(dat, nyears >= 0.5)
dat$myc <- with(dat, ifelse(Myc=="ECM", 100,0))

#################### META FOREST #################
X.abs <- dat[, c("abs","abs.var", "N", "amb", "biomass","myc","Depth..cm.")] %>% 
  rename(yi=abs, vi=abs.var, "Nitrogen.fertilization"=N, "Effect.biomass"=biomass, "Cstock" = amb, "Soil.depth"=Depth..cm.)
#Set random seed
set.seed(4)
forest.abs <- MetaForest(yi ~ ., 
                         data = X.abs,
                         whichweights = "unif", mtry = 3, min.node.size = 2,
                         num.trees = 5000)
forest.inbag <- MetaForest(yi ~ ., 
                         data = X.abs,
                         whichweights = "unif", mtry = 3, min.node.size = 2,
                         keep.inbag = TRUE,
                         num.trees = 5000)
forest.abs$forest$r.squared
plot(forest.abs)
VarImpPlot(forest.abs)
# Raster layers
biomass <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2relEffect.tif")
biomass[is.na(biomass)] <- 0
antiperc <- function(x) log(x/100 + 1)
biomass <- antiperc(biomass)
soc <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_gm2.tif")
fert <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_Nfertilization.tif")
ecm <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrEM_current_all.tif")
er <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrER_current_all.tif")
am <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrAM_current_all.tif")
nm <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrNM_current_all.tif")
myc <- ecm+er # SUM ECM and Ericoid
myc <- resample(myc,biomass,method="bilinear")
s<- stack(biomass, soc, fert, myc)
s.df <- as.data.frame(s,xy=TRUE) %>% rename("Nitrogen.fertilization"=ESA_Nfertilization_category, "Effect.biomass"=layer.1, 
                                            "Cstock" = SOC_015cm_aggregated0p25_gm2, "myc" = layer.2) %>%
  mutate(Nitrogen.fertilization=factor(Nitrogen.fertilization), Soil.depth=20)

#test<- data.frame(Effect.biomass=c(01,0,0.25), Cstock=c(1000,5000,10000), Nitrogen.fertilization=c("Nhigh","Nhigh","low"))
#test<- data.frame(Effect.biomass=c(rep(01,10),rep(0,10),rep(0.25,10)), Cstock=rep(1000,30), Nitrogen.fertilization=rep("Nhigh",30))
#predict(forest.inbag, data = test, type="se")$predictions

r.dfNA <- s.df[complete.cases(s.df),]
RFpred <- predict(forest.abs, data = r.dfNA)$predictions
#RFpredSE <- predict(forest.inbag, data = r.dfNA, type="se")$predictions
absES <- cbind(r.dfNA[c("x","y")], RFpred)
absES <- left_join(s.df,absES)
absES <- rasterFromXYZ(absES[,c("x", "y", "RFpred")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(absES)
#absSE <- cbind(r.dfNA[c("x","y")], RFpredSE)
#absSE <- left_join(s.df,absSE)
#absSE <- rasterFromXYZ(absSE[,c("x", "y", "RFpredSE")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#plot(absSE)
cellStats(absES,"mean", na.rm=T) # g/m2
writeRaster(absES,"maps/CO2abslEffect_RF_gm2.tif",format="GTiff",overwrite=TRUE)
absEStha <- absES*0.01 # Mg/ha == tonnes/ha. I need to compute the total Mg in the world, so I need Mg/pixel:
writeRaster(absEStha,"maps/CO2abslEffect_RF_tha.tif",format="GTiff",overwrite=TRUE)
a <- area(absEStha) # get area of projected raster.
absESpixel <- absEStha * a * 100 # area is in km2 multiply by 100 to get ha 
cellStats(absESpixel,"sum", na.rm=T) * 10^(-9)   # Pg soil C at 0-30cm
writeRaster(absESpixel,"maps/CO2abslEffect_RF_pixel.tif",format="GTiff",overwrite=TRUE)


