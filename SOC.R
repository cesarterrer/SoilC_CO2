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

