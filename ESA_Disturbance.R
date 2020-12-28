library(raster)
esa.new <- raster("~/OneDrive - LLNL/IIASA/maps/ESA_2012_aggregated0p25.tif")
r <- as(esa.new, "SpatialPixelsDataFrame")
str(r@data)
head(r@data)
levels(factor(r@data$ESA_2012_aggregated0p25))

legend <- read.csv("ESA_Disturbance.csv")

r <- merge(r, legend, by.x="ESA_2012_aggregated0p25", by.y="NB_LAB")

r.ras <- raster (r,"ESAagg")
plot(r.ras)

writeRaster(r.ras,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_Disturbance.tif",overwrite=TRUE)
