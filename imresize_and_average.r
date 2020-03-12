rm(list=ls())

library(R.matlab)
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
save(r_mean, file = 'Delta_cveg_de_Delta_Csoil_mean.rda')
save(r_mean, file = 'Delta_cveg_de_Delta_Csoil_sd.rda')
save(r_median, file = 'Delta_cveg_de_Delta_Csoil_median.rda')

##----------------------------------------------------------------
##----------------------------------------------------------------
##----compute the difference between Csoil and Cveg/Ratio---------
##----------------------------------------------------------------
##----------------------------------------------------------------
##----------------------------------------------------------------

delta_cveg<-raster('J:/Imperial_college/Master_Project/cesar_files/CO2absEffect_TotalBiomass_tha.tif')
a <- area(delta_cveg)
map_MgPixel <- delta_cveg * (a * 100)
cellStats(map_MgPixel,"sum", na.rm=T) * 10^(-9) 
#make sure it is around 58 PgC

delta_csoil<-raster('J:/Imperial_college/Master_Project/cesar_files/CO2abslEffect_RF_tha.tif')
a <- area(delta_csoil)
map_MgPixel <- delta_csoil * (a * 100)
cellStats(map_MgPixel,"sum", na.rm=T) * 10^(-9) 
#make sure it is around 38 PgC

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
