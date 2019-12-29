# SOC stock in tonnes per ha from SoilGrids aggregated at 1km
# https://files.isric.org/soilgrids/data/aggregated/10km/
library(raster)

d05 <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/OCSTHA_M_sd1_10km_ll.tif")
d515 <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/OCSTHA_M_sd2_10km_ll.tif")
biomass <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2relEffect.tif")

d015 <- (d05 + d515) * 100 # g/m2
d015_aggregated0p25_gm2 <- resample(d015,biomass, method="bilinear")

writeRaster(d015,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_1km.tif")
writeRaster(d015_aggregated0p25_gm2,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_gm2.tif")

d1530 <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/OCSTHA_M_sd3_10km_ll.tif")
d030 <- (d05 + d515 + d1530) * 100 # g/m2
d030_aggregated0p25_gm2 <- resample(d030,biomass, method="bilinear")
writeRaster(d030_aggregated0p25_gm2,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_030cm_aggregated0p25_gm2.tif",overwrite=TRUE)

#### SAME SOURCE, DIFFERENT DEPTHS and WEB #####
# https://zenodo.org/record/2536040#.XgasES2ZN24
soc30<-raster("/Users/terrermoreno1/Downloads/sol_organic.carbon.stock_msa.kgm2_m_250m_b0..30cm_2015_v0.2.tif") # kg/m2 250m
soc30_aggregated0p25_gm2 <- resample(soc30,biomass, method="bilinear")
soc30_aggregated0p25_gm2 <- soc30_aggregated0p25_gm2*1000
writeRaster(soc30_aggregated0p25_gm2,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_030cm_aggregated0p25_gm2_zenodo.tif",overwrite=TRUE)

soc10<-raster("/Users/terrermoreno1/Downloads/sol_organic.carbon.stock_msa.kgm2_m_250m_b0..10cm_1950..2017_v0.2.tif") # kg/m2 250m
soc10_gm2 <- soc10*1000
soc10_aggregated0p25_gm2 <- resample(soc10_gm2,biomass, method="bilinear")
writeRaster(soc10_aggregated0p25_gm2,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_010cm_aggregated0p25_gm2_zenodo.tif",overwrite=TRUE)

