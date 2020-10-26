library(raster)
plus <- function(x) {
  if(all(is.na(x))){
    c(x[0],NA)} else {
      sum(x,na.rm = TRUE)}
}
library(R.matlab)


###-------------------------------------------------------------------------
###-------------------------------------------------------------------------
###                                                                      ---
###                                 REDO WITH TRENDY V7                  ---
###                                                                      ---
###-------------------------------------------------------------------------
###-------------------------------------------------------------------------

rm(list=ls())
plus <- function(x) {
  if(all(is.na(x))){
    c(x[0],NA)} else {
      sum(x,na.rm = TRUE)}
}


setwd("E:/Cesar_project_csoil/processed_trendy_v7/model_to_use") #This is a folder with nc files shared by Beni
#mostly 


Models_output<-list.files(pattern = "*cVeg_FINAL_MEAN.nc$") #list all the nc files sent by Beni
#WE only selected "CABLE"        "CLM"          "ISAM"         "LPJ-GUESS"    "ORCHIDEE"     "ORCHIDEE-CNP"
#This is to be consistent with Figure 3, reason of selection was explained in text of the paper
model_name_list<-unlist(lapply(strsplit(Models_output,split = "_"),function(x) x[[1]]))
rotate_it <- function(x) t(apply(x, 2, rev)) #self-defined function for rotating matrix
ReadMatAndMakeNiceMap<-function(xy){
  Model_name<-strsplit(xy,split = "_")
  Model_name<-Model_name[[1]][[1]]
  Models_output_cVeg_final_mean<-raster(paste0(Model_name,"_S1_cVeg_FINAL_MEAN.nc"))
  Models_output_cveg_Init_mean<-raster(paste0(Model_name,"_S1_cVeg_INIT_MEAN.nc"))
  Models_output_Csoil_final_mean<-raster(paste0(Model_name,"_S1_cSoil_FINAL_MEAN.nc"))
  Models_output_Csoil_Init_mean<-raster(paste0(Model_name,"_S1_cSoil_INIT_MEAN.nc"))
  Delta_cveg_de_Delta_Csoil<-(Models_output_cVeg_final_mean-Models_output_cveg_Init_mean)/(Models_output_Csoil_final_mean-Models_output_Csoil_Init_mean)
  return(Delta_cveg_de_Delta_Csoil)
}

for(i in 1:length(Models_output)) {
  Cesar_map<-raster("E:/Cesar_project_csoil/Csoil_data/CO2abslEffect_RF_tha.tif") #Read Cesar's file as a template (for resolution ,extent etc)
  My_Map<-ReadMatAndMakeNiceMap(Models_output[i])
  if (My_Map@extent[1]<2 & My_Map@extent[1]>-2) {
    My_Map<-rotate(My_Map)
  }
  Resize_map <- resample(My_Map, Cesar_map)#resize the image  to 720*1440
  if (i==1){
    Model_stack<-Resize_map #stack them together
  }else {
    Model_stack<-stack(Model_stack,Resize_map)
  }
}
names(Model_stack) <- model_name_list
r_mean.abs <- calc(Model_stack,mean,na.rm = TRUE) #Calculate mean of the ratio across models
r_median.abs <- calc(Model_stack,median,na.rm = TRUE) #Calculate median of the ratio across models
r_sd.abs <- calc(Model_stack, sd,na.rm = TRUE) #Calculate standard deviation across models

##---------------------------------------------------------------
##                percenatge delta Cveg and Csoil               -
##      delta Cveg = (Cveg_after - Cveg_before)/Cveg_before     -
##---------------------------------------------------------------


setwd("E:/Cesar_project_csoil/processed_trendy_v7/model_to_use") #This is a folder with Trendy v7 nc files shared by Beni
#mostly 


Models_output<-list.files(pattern = "*cVeg_FINAL_MEAN.nc$") #list all the nc files sent by Beni
#WE only selected "CABLE"        "CLM"          "ISAM"         "LPJ-GUESS"    "ORCHIDEE"     "ORCHIDEE-CNP"
#This is to be consistent with Figure 3, reason of selection was explained in text of the paper
model_name_list<-unlist(lapply(strsplit(Models_output,split = "_"),function(x) x[[1]]))
rotate_it <- function(x) t(apply(x, 2, rev)) #self-defined function for rotating matrix
ReadMatAndMakeNiceMap<-function(xy){
  Model_name<-strsplit(xy,split = "_")
  Model_name<-Model_name[[1]][[1]]
  Models_output_cVeg_final_mean<-raster(paste0(Model_name,"_S1_cVeg_FINAL_MEAN.nc"))
  Models_output_cveg_Init_mean<-raster(paste0(Model_name,"_S1_cVeg_INIT_MEAN.nc"))
  Models_output_Csoil_final_mean<-raster(paste0(Model_name,"_S1_cSoil_FINAL_MEAN.nc"))
  Models_output_Csoil_Init_mean<-raster(paste0(Model_name,"_S1_cSoil_INIT_MEAN.nc"))
  
  Delta_cveg<-(Models_output_cVeg_final_mean-Models_output_cveg_Init_mean)/Models_output_cveg_Init_mean
  Delta_Csoil<-(Models_output_Csoil_final_mean-Models_output_Csoil_Init_mean)/Models_output_Csoil_Init_mean
  Delta_cveg_de_Delta_Csoil<-Delta_cveg/Delta_Csoil
  return(Delta_cveg_de_Delta_Csoil)
}

for(i in 1:length(Models_output)) {
  Cesar_map<-raster("E:/Cesar_project_csoil/Csoil_data/CO2abslEffect_RF_tha.tif") #Read Cesar's file as a template (for resolution ,extent etc)
  My_Map<-ReadMatAndMakeNiceMap(Models_output[i])
  if (My_Map@extent[1]<2 & My_Map@extent[1]>-2) {
    My_Map<-rotate(My_Map)
  }
  Resize_map <- resample(My_Map, Cesar_map)#resize the image  to 720*1440
  if (i==1){
    Model_stack<-Resize_map #stack them together
  }else {
    Model_stack<-stack(Model_stack,Resize_map)
  }
}
names(Model_stack) <- model_name_list
r_mean.rel <- calc(Model_stack,mean,na.rm = TRUE) #Calculate mean of the ratio across models
r_median.rel <- calc(Model_stack,median,na.rm = TRUE) #Calculate median of the ratio across models
r_sd.rel <- calc(Model_stack, sd,na.rm = TRUE) #Calculate standard deviation across models

save(r_mean.abs,r_median.abs,r_sd.abs,r_mean.rel,r_median.rel,r_sd.rel, file = "Delta_cveg_de_Delta_Csoil_mean_Trendy_v7.rda")

load("maps/Delta_cveg_de_Delta_Csoil_mean_Trendy_v7.rda")
############################## DIFFERENCE ##################################
Cveg.abs <- raster("maps/Bbiomass_tha.tif")
Cveg.abs[is.na(Cveg.abs)] <- 1
Csoil.abs <- raster("maps/CO2abslEffect_RF_tha.tif")  # Mg/ha
Cveg.abs <- mask(Cveg.abs, Csoil.abs)
#my_ratio<- Cveg.abs/Csoil.abs
Csoil_expected.abs<-Cveg.abs/r_mean.abs
Csoil_expected.abs[Csoil_expected.abs>quantile(Csoil_expected.abs,.99)]<-quantile(Csoil_expected.abs,.99)
Csoil_expected.abs[Csoil_expected.abs<quantile(Csoil_expected.abs,.01)]<-quantile(Csoil_expected.abs,.01)
stack.abs<- stack(-Csoil.abs,Csoil_expected.abs)
dif.abs <- calc(stack.abs,plus)
writeRaster(dif.abs,"Diff_obs_exp_tha_trendyv7.tif",format="GTiff",overwrite=TRUE)

# Relative
load("maps/CMIP_SOC_2002_kgc_m2.rda")
Cveg.rel <- make_pct(raster("maps/Bbiomass.tif")) # %
Cveg.rel[is.na(Cveg.rel)] <- 1
Csoil.rel <- (Csoil.abs*100)/(CMIP_SOC*10)  # %
Csoil.rel <- raster::calc(Csoil.rel, fun= function(x) ifelse (x>quantile(Csoil.rel,.95),quantile(Csoil.rel,.95),x)) 
Cveg.rel <- mask(Cveg.rel, Csoil.rel)
Csoil_expected.rel<-Cveg.rel/r_median.rel
Csoil_expected.rel[Csoil_expected.rel>quantile(Csoil_expected.rel,.99)]<-quantile(Csoil_expected.rel,.99)
Csoil_expected.rel[Csoil_expected.rel<quantile(Csoil_expected.rel,.01)]<-quantile(Csoil_expected.rel,.01)
stack.rel<- stack(-Csoil.rel,Csoil_expected.rel)
dif.rel <- calc(stack.rel,plus)
writeRaster(dif.rel,"Diff_obs_exp_perc_trendyv7.tif",format="GTiff",overwrite=TRUE)

