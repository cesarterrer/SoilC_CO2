library(raster)
plus <- function(x) {
  if(all(is.na(x))){
    c(x[0],NA)} else {
      sum(x,na.rm = TRUE)}
}
library(R.matlab)

Models_output<-list.files("maps/CMIP/absolute",pattern = "*.mat",recursive = TRUE, full.names=TRUE) #list all the output files from CMIP5_CVEG_CSOIL_RATIO.m; One model each file
rotate_it <- function(x) t(apply(x, 2, rev)) #self-defined function for rotating matrix

for(i in 1:length(Models_output)) {
  Letssee<-readMat(Models_output[i]) #Read the mat file as a matrix
  My_Map<-raster(rotate_it(Letssee[[1]])) #convert to raster and rotate 90 degress
  My_Map<-flip(flip(My_Map,2),1) #flip it over to the correct angle
  extent(My_Map) <- extent(0, 360, -90, 90) #adjust its extent
  My_Map <- rotate(My_Map) 
  Cesar_map<-raster("maps/CO2abslEffect_RF_tha.tif") 
  Resize_map <- resample(My_Map, Cesar_map)#resize the image  to 720*1440
  if (i==1){
    Model_stack<-Resize_map #stack them together
  }else {
    Model_stack<-stack(Model_stack,Resize_map)
  }
}
names(Model_stack) <- c( "CanESM2.mat","GFDL-ESM2M.mat","HadGEM2-ES.mat","IPSL-CM5A-LR.mat","MPI-ESM-LR.mat","NorESM1-ME.mat")
r_mean.abs <- calc(Model_stack,mean,na.rm = TRUE) #Calculate mean of the ratio across models
r_median.abs <- calc(Model_stack,median,na.rm = TRUE) #Calculate median of the ratio across models
r_sd.abs <- calc(Model_stack, sd,na.rm = TRUE) #Calculate standard deviation across models


##---------------------------------------------------------------
##                percenatge delta Cveg and Csoil               -
##      delta Cveg = (Cveg_after - Cveg_before)/Cveg_before     -
##---------------------------------------------------------------
Models_output<-list.files("maps/CMIP/relative",pattern = "*tage.mat",recursive = TRUE,full.names=TRUE) #list all the output files from CMIP5_CVEG_CSOIL_RATIO.m; One model each file
rotate_it <- function(x) t(apply(x, 2, rev)) #self-defined function for rotating matrix

for(i in 1:length(Models_output)) {
  Letssee<-readMat(Models_output[i]) #Read the mat file as a matrix
  My_Map<-raster(rotate_it(Letssee[[1]])) #convert to raster and rotate 90 degress
  My_Map<-flip(flip(My_Map,2),1) #flip it over to the correct angle
  extent(My_Map) <- extent(0, 360, -90, 90) #adjust its extent
  My_Map <- rotate(My_Map) #to align with Cesar's map
  Cesar_map<-raster("maps/CO2abslEffect_RF_tha.tif")
  Resize_map <- resample(My_Map, Cesar_map)#resize the image  to 720*1440
  if (i==1){
    Model_stack<-Resize_map #stack them together
  }else {
    Model_stack<-stack(Model_stack,Resize_map)
  }
}
names(Model_stack) <- c( "CanESM2.mat","GFDL-ESM2M.mat","HadGEM2-ES.mat","IPSL-CM5A-LR.mat","MPI-ESM-LR.mat","NorESM1-ME.mat")
r_mean.rel <- calc(Model_stack,mean,na.rm = TRUE) #Calculate mean of the ratio across models
r_median.rel <- calc(Model_stack,median,na.rm = TRUE) #Calculate median of the ratio across models
r_sd.rel <- calc(Model_stack, sd,na.rm = TRUE) #Calculate standard deviation across models

############################## DIFFERENCE ##################################
# Absolute
Cveg.abs <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect_TotalBiomass_tha.tif")
Csoil.abs <- raster("maps/CO2abslEffect_RF_tha.tif")  # Mg/ha
Csoil_expected.abs<-Cveg.abs/r_median.abs
Csoil_expected.abs[Csoil_expected.abs>cellStats(Csoil.abs,max)]<-cellStats(Csoil.abs,max)
Csoil_expected.abs[Csoil_expected.abs<cellStats(Csoil.abs,min)]<-cellStats(Csoil.abs,min)
stack.abs<- stack(Csoil.abs,-Csoil_expected.abs)
dif.abs <- calc(stack.abs,plus)
writeRaster(dif.abs,"maps/Diff_obs_exp_tha.tif",format="GTiff",overwrite=TRUE)

# Relative
load("maps/CMIP_SOC_2002_kgc_m2.rda")
Cveg.rel <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2relEffect.tif") # %
Csoil.rel <- (Csoil.abs*100)/(CMIP_SOC*10)  # %
Csoil.rel <- raster::calc(Csoil.rel, fun= function(x) ifelse (x>quantile(Csoil.rel,.95),quantile(Csoil.rel,.95),x)) 
Csoil_expected.rel<-Cveg.rel/r_mean.rel
Csoil_expected.rel[Csoil_expected.rel>cellStats(Csoil.rel,max)]<-cellStats(Csoil.rel,max)
Csoil_expected.rel[Csoil_expected.rel<cellStats(Csoil.rel,min)]<-cellStats(Csoil.rel,min)
stack.rel<- stack(Csoil.rel,-Csoil_expected.rel)
dif.rel <- calc(stack.rel,plus)
writeRaster(dif.rel,"maps/Diff_obs_exp_perc.tif",format="GTiff",overwrite=TRUE)


