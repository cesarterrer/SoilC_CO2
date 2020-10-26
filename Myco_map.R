ecm <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrEM_current_all.tif")
#er <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrER_current_all.tif")
#am <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrAM_current_all.tif")
#nm <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/myco_maps_Dec2017/MycDistrNM_current_all.tif")
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
ecotype <- raster("~/OneDrive - LLNL/IIASA/maps/ESA_EcosystemType.tif")
s<- stack(myc,ecotype)
rc <- function(x1,x2) {
  ifelse( x2 == 1,  1, ifelse( x2 == 2, 1, x1 ))
}

myc2 <- overlay(s, fun=rc)
plot(myc2)
writeRaster(myc2,"maps/Myco_AMEMeco.tif",format="GTiff",overwrite=TRUE)
