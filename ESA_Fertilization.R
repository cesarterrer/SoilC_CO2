# ESA land cover map 300m in 2012, Released 2016 https://www.esa-landcover-cci.org/?q=node/169
library(raster)
esa <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA land cover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2012-v2.0.7.tif")
biomass <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2relEffect.tif")

esa.new <- resample(esa, biomass, method = 'ngb') # include method ngb to resample to a new resolution
writeRaster(esa.new,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_2012_aggregated0p25.tiff")

esa.new <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_2012_aggregated0p25.tif")
r <- as(esa.new, "SpatialPixelsDataFrame")
str(r@data)
head(r@data)
levels(factor(r@data$ESA_2012_aggregated0p25))

legend <- read.csv("ESA_Fert.csv")

r <- merge(r, legend, by.x="ESA_2012_aggregated0p25", by.y="NB_LAB")

r.ras <- raster (r,"ESAagg")
plot(r.ras)

writeRaster(r.ras,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_Nfertilization.tif",overwrite=TRUE)
