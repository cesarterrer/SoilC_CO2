##### TOTAL BIOMASS #####
biomass <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect.tif")  # Mg
biomass.se <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absSE.tif")

esa.pan <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/ESA_PANratios.tif")
total.s <- stack(esa.pan,biomass,biomass.se)
total.df <- as.data.frame(total.s,xy=TRUE)
#Calculate the increase in TOTAL BIOMASS using TB ratios from Liu et al.
library("readxl")
ratios <- read_excel("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/TBratio.xlsx", col_types = c("text", "numeric", "numeric", "guess", "guess"))
total.df <- merge(total.df,ratios, "ESA_PANratios_category")
total.df$TotBiom <- round(total.df$CO2absEffect * total.df$TBratio,2)
total.df$TotBiomSE <- round(total.df$CO2absSE * total.df$TBratio,2)
total.biomass <- rasterFromXYZ(total.df[,c("x", "y", "TotBiom")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
cellStats(total.biomass,"sum", na.rm=T) * 10^(-9)
total.biomass.se <- rasterFromXYZ(total.df[,c("x", "y", "TotBiomSE")],crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
writeRaster(total.biomass,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect_TotalBiomass.tif",overwrite=TRUE)
writeRaster(total.biomass.se,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absSE_TotalBiomass.tif",overwrite=TRUE)

total.biomass.a <- area(total.biomass)
total.biomasstha <- overlay(total.biomass, total.biomass.a, fun=function(x,y) x/(y*100)) # Mg/ha
total.biomasstha.se <- overlay(total.biomass.se, total.biomass.a, fun=function(x,y) x/(y*100)) # Mg/ha
writeRaster(total.biomasstha,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect_TotalBiomass_tha.tif",overwrite=TRUE)
writeRaster(total.biomasstha.se,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/Upscaling_Biomass/Maps/CO2absEffect_TotalBiomass.SE_tha.tif",overwrite=TRUE)
