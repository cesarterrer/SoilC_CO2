rm(list=ls())
library(R.matlab)
library(raster)
plus <- function(x) {
  if(all(is.na(x))){
    c(x[0],NA)} else {
      sum(x,na.rm = TRUE)}
}
load("Delta_cveg_de_Delta_Csoil_median.rda")
########################################################################################################################
########################################################################################################################
###                                                                                                                  ###
###                                                    2020-03-01                                                    ###
###                                       Contact:huanyuan.zhang@ouce.ox.ac.uk                                       ###
###                                           This is prepared for Cesar,                                            ###
###         in orther to resize some maps produced by CMIP5_CVEG_CSOIL_RATIO.m, and compute mean and standard error  ###
###                                                                                                                  ###
########################################################################################################################
########################################################################################################################
##############
##############
##############
#
#setwd(....) #change working directory to where the mat files were stored 
#Also need to change the address of tif file
##############
##############
##############


##---------------------------------------------------------------
##                absolute delta Cveg and Csoil                 -
##            delta Cveg = Cveg_after - Cveg_before             -
##---------------------------------------------------------------

Models_output<-list.files(pattern = "*.mat",recursive = TRUE) #list all the output files from CMIP5_CVEG_CSOIL_RATIO.m; One model each file
rotate_it <- function(x) t(apply(x, 2, rev)) #self-defined function for rotating matrix

for(i in 1:length(Models_output)) {
Letssee<-readMat(Models_output[i]) #Read the mat file as a matrix
My_Map<-raster(rotate_it(Letssee[[1]])) #convert to raster and rotate 90 degress
My_Map<-flip(flip(My_Map,2),1) #flip it over to the correct angle
extent(My_Map) <- extent(0, 360, -90, 90) #adjust its extent
My_Map <- rotate(My_Map) #to align with Cesar's map
Cesar_map<-raster("F:/Cesar_project_csoil/Csoil_data/CO2abslEffect_RF_tha.tif") #Read Cesar's file as a template (for resolution ,extent etc)
Resize_map <- resample(My_Map, Cesar_map)#resize the image  to 720*1440
if (i==1){
  Model_stack<-Resize_map #stack them together
}else {
  Model_stack<-stack(Model_stack,Resize_map)
}
}
names(Model_stack) <- c( "CanESM2.mat","GFDL-ESM2M.mat","HadGEM2-ES.mat","IPSL-CM5A-LR.mat","MPI-ESM-LR.mat","NorESM1-ME.mat")
r_mean <- calc(Model_stack,mean,na.rm = TRUE) #Calculate mean of the ratio across models
r_median <- calc(Model_stack,median,na.rm = TRUE) #Calculate median of the ratio across models
r_sd <- calc(Model_stack, sd,na.rm = TRUE) #Calculate standard deviation across models
#save(r_mean, file = 'Delta_cveg_de_Delta_Csoil_mean.rda')
#save(r_sd, file = 'Delta_cveg_de_Delta_Csoil_sd.rda')
#save(r_median, file = 'Delta_cveg_de_Delta_Csoil_median.rda')


##---------------------------------------------------------------
##                percenatge delta Cveg and Csoil               -
##      delta Cveg = (Cveg_after - Cveg_before)/Cveg_before     -
##---------------------------------------------------------------
Models_output<-list.files(pattern = "*tage.mat",recursive = TRUE) #list all the output files from CMIP5_CVEG_CSOIL_RATIO.m; One model each file
rotate_it <- function(x) t(apply(x, 2, rev)) #self-defined function for rotating matrix

for(i in 1:length(Models_output)) {
  Letssee<-readMat(Models_output[i]) #Read the mat file as a matrix
  My_Map<-raster(rotate_it(Letssee[[1]])) #convert to raster and rotate 90 degress
  My_Map<-flip(flip(My_Map,2),1) #flip it over to the correct angle
  extent(My_Map) <- extent(0, 360, -90, 90) #adjust its extent
  My_Map <- rotate(My_Map) #to align with Cesar's map
  Cesar_map<-raster("F:/Cesar_project_csoil/Csoil_data/CO2abslEffect_RF_tha.tif") #Read Cesar's file as a template (for resolution ,extent etc)
  Resize_map <- resample(My_Map, Cesar_map)#resize the image  to 720*1440
  if (i==1){
    Model_stack<-Resize_map #stack them together
  }else {
    Model_stack<-stack(Model_stack,Resize_map)
  }
}
names(Model_stack) <- c( "CanESM2.mat","GFDL-ESM2M.mat","HadGEM2-ES.mat","IPSL-CM5A-LR.mat","MPI-ESM-LR.mat","NorESM1-ME.mat")
r_mean <- calc(Model_stack,mean,na.rm = TRUE) #Calculate mean of the ratio across models
r_median <- calc(Model_stack,median,na.rm = TRUE) #Calculate median of the ratio across models
r_sd <- calc(Model_stack, sd,na.rm = TRUE) #Calculate standard deviation across models
#save(r_mean, file = 'Delta_cveg_de_Delta_Csoil_mean%.rda')
#save(r_sd, file = 'Delta_cveg_de_Delta_Csoil_sd%.rda')
#save(r_median, file = 'Delta_cveg_de_Delta_Csoil_median%.rda')


##----------------------------------------------------------------
##----------------------------------------------------------------
##----compute the difference between Csoil and Cveg/Ratio---------
##----------------------------------------------------------------
##----------------------------------------------------------------
##----------------------------------------------------------------
cuts=c(20,0,-20)
pal <- colorRampPalette(c("grey","black")) #for plotting the map


# Absolute
load("Delta_cveg_de_Delta_Csoil_median.rda")
Cveg.abs <- raster("J:/Imperial_college/Master_Project/cesar_files/CO2absEffect_TotalBiomass_tha.tif")
Csoil.abs <- raster("F:/Cesar_project_csoil/Csoil_data/CO2abslEffect_RF_tha.tif")  # Mg/ha
Csoil_expected.abs<-Cveg.abs/r_median
Csoil_expected.abs[Csoil_expected.abs>cellStats(Csoil.abs,max)]<-cellStats(Csoil.abs,max)
Csoil_expected.abs[Csoil_expected.abs<cellStats(Csoil.abs,min)]<-cellStats(Csoil.abs,min)
stack.abs<- stack(Csoil.abs,-Csoil_expected.abs)
dif.abs <- calc(stack.abs,plus)
plot(dif.abs, breaks=cuts,col=pal(3),main='based on absolute')

# Relative
load("Delta_cveg_de_Delta_Csoil_median%.rda")
Cveg.rel <- raster("J:/Imperial_college/Master_Project/cesar_files/CO2relEffect_above_biomass.tif") # %
Csoil.rel <- raster("J:/Imperial_college/Master_Project/cesar_files/CO2relEffect_RF_csoil_tha.tif")  # %
Csoil_expected.rel<-Cveg.rel/r_median
Csoil_expected.rel[Csoil_expected.rel>cellStats(Csoil.rel,max)]<-cellStats(Csoil.rel,max)
Csoil_expected.rel[Csoil_expected.rel<cellStats(Csoil.rel,min)]<-cellStats(Csoil.rel,min)
stack.rel<- stack(Csoil.rel,-Csoil_expected.rel)
dif.rel <- calc(stack.rel,plus)
plot(dif.rel, breaks=cuts,col=pal(3),main='based on %')




















##----------------------------------------------------------------
##                      legacy code archived (please ignore)     -
##----------------------------------------------------------------
Expected_delta_csoil<-delta_cveg/r_median
Expected_delta_csoil[Expected_delta_csoil>30]<-30
#This is important because:
#Ratio = ΔCveg / ΔCsoil, 
#ΔCsoil_expected = ΔCveg_Cesar / Ratio
#However, there are lots of tiny ΔCveg in CMIP models which would make a crazily huge ΔCsoil_expected
# we set it as 30 because values in the map of delta_csoil are smaller than 10.

Expected_delta_csoil[Expected_delta_csoil<(0-1)]<-(0-1)
# Again, there are lots of ΔCveg in CMIP models around -0.00001 which results in crazily negative Expected_delta_csoil
# set as -1 because the minimum in delta_csoil is about -1
a <- area(Expected_delta_csoil)
map_MgPixel <- Expected_delta_csoil * (a * 100)
cellStats(map_MgPixel,"sum", na.rm=T) * 10^(-9) 
#for median, it is 42 PgC, I prefered this one because it made me feel like safe estimation
#for mean, it is 32 PgC

Higher_than_expected<-delta_csoil-Expected_delta_csoil
#this will reveal the difference, positive value denote: Csoil map higher than ratio*Cveg ma

cuts=c(20,0,-20)
pal <- colorRampPalette(c("grey","black"))
plot(Expected_delta_csoil)

plot(delta_csoil,)
plot(delta_cveg)
plot(Higher_than_expected, breaks=cuts,col=pal(3),main= "positive value denote: Csoil map higher than ratio*Cveg map")
plot(r_mean)
plot(r_median)
