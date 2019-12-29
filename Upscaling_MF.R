library(devtools)
install_github("cjvanlissa/metaforest")
library(metaforest)
library(tidyverse)
library(metafor)
library(raster)
library(caret)
make_pct <- function(x) (exp(x) - 1) * 100
antiperc <- function(x) log(x/100 + 1)
#####################
source("Effect Size.R")
dat <- filter(dat, biomass != "NA", nyears >= 0.5, Myc != "NM")
data.abs <- dat %>% rename ("Ecosystem.type"=Ecosystem.type , "Duration"=nyears,  "Experiment.type"=Experiment_type,
                        "Effect.biomass"=biomass, "Symbiotic.type"=Myc, "Nitrogen.fertilization"=N, "Soil.depth"=Depth..cm.) %>%
  mutate( Cstock = amb) %>%
  dplyr::select(-yi, -vi) %>% rename(yi=abs, vi=abs.var) %>% droplevels()
#################### META FOREST #################
mf_cv_abs <- readRDS("mf_cv_abs.RData")
mf_cv_abs
forest.abs <- mf_cv_abs$finalModel
VarImpPlot(forest.abs)
selected_mods <- readRDS("preselect_mods.RData")
set.seed(4)
forest.abs <- MetaForest(as.formula(paste0("yi~", paste(selected_mods, collapse = "+"))),
                         data = data.abs,
                         study = "Site",
                         whichweights = "unif", mtry = 3, min.node.size = 6,
                         num.trees = 10000)
VarImpPlot(forest.abs)
forest.abs <- MetaForest(yi ~ Effect.biomass + Cstock + Soil.depth + Symbiotic.type + Nitrogen.fertilization + 
                           Experiment.type + Ecosystem.type, 
                         data = data.abs,
                         #study = "Site",
                         whichweights = "unif", mtry = 4, min.node.size = 4,
                         num.trees = 10000)
VarImpPlot(forest.abs)
PartialDependence(forest.abs)

# Raster layers
biomass <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2relEffect.tif")
biomass[is.na(biomass)] <- 0
antiperc <- function(x) log(x/100 + 1)
biomass <- antiperc(biomass)
lai <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/LAImax_2012_aggregated0p25.tif")
soc <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_gm2.tif")
fert <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_Nfertilization.tif")
disturbance <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_Disturbance.tif")
ecotype <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_EcosystemType.tif")
ecm <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrEM_current_all.tif")
er <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrER_current_all.tif")
am <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrAM_current_all.tif")
nm <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrNM_current_all.tif")
Myc <- stack(am,ecm,er,nm)
Myc.df <-as.data.frame(Myc,xy=TRUE) %>% rename(AM=MycDistrAM_current_all,ECM=MycDistrEM_current_all, 
                                               ER=MycDistrER_current_all, NM=MycDistrNM_current_all)
Myc.df <- Myc.df %>% rownames_to_column('id') %>%
  left_join(Myc.df %>% rownames_to_column('id') %>%
      pivot_longer(names_to="max_myc", values_to="max_cnt", cols=AM:NM) %>% 
      group_by(id) %>% 
      slice(which.max(max_cnt))) 
#Myc.df$max_myc <- with(Myc.df, ifelse(max_cnt==0, NA, max_myc))
Myc.df$max_myc <- factor(Myc.df$max_myc)
lev <- levels(Myc.df$max_myc)
Myc.df$max_myc <- as.numeric(Myc.df$max_myc)
myc <- rasterFromXYZ(Myc.df[,c("x", "y", "max_myc")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
myc <- resample(myc,biomass,method="ngb")
levels(myc) <- data.frame(ID = 1:length(lev), code = lev)

s<- stack(biomass, lai, soc, fert, myc, disturbance, ecotype)
s.df <- as.data.frame(s,xy=TRUE) %>% rename("Nitrogen.fertilization"=ESA_Nfertilization_category, "Effect.biomass"=layer, 
                                            "Cstock" = SOC_015cm_aggregated0p25_gm2, "Symbiotic.type" = max_myc_code, "LAImax"=LAImax_2012_aggregated0p25,
                                            "Disturbance"=ESA_Disturbance_category, "Ecosystem.type"=ESA_EcosystemType_category) %>%
  mutate(Nitrogen.fertilization=factor(Nitrogen.fertilization), 
         Symbiotic.type=factor(Symbiotic.type),
         Soil.depth=30,
         Duration=mean(dat$nyears),
         Experiment.type=factor("FACE", levels=c("Chamber","FACE","OTC")),
         Disturbance=factor(Disturbance),
         Ecosystem.type=factor(Ecosystem.type))

r.dfNA <- s.df[complete.cases(s.df),]

## PREDICT ##
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


