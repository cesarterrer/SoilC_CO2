library(raster)

df <- read.csv("~/Documents/SoilC_CO2/soilC - combined.csv",na.strings = "")
df$obs <- 1:nrow(df)
dat = df %>% filter(!is.na(Latitude)) %>% dplyr::rename(biomass=yi)  # Remove empty rows
dat$LAT <- ifelse(dat$Lat %in% "S",dat$Latitude * -1,dat$Latitude)
dat$LONG <- ifelse(dat$Lon %in% "W",dat$Longitude * -1,dat$Longitude)

points <- dplyr::select(dat,obs, LAT,LONG) 
coordinates(points) <- c("LONG","LAT")
projection(points) <- CRS ("+proj=longlat +ellps=WGS84")


### MAX MAP 1901-2017 ###
cruMAPmax <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/MAP.max.1901.2017.tif")
cruMAPmax.df <- data.frame(obs=points$obs, MAPmax=raster::extract(cruMAPmax,points))
new <- left_join(df, cruMAPmax.df)
new$MAP2 <- new$MAPraw
new$MAP2[new$MAP2 == "watered"] <- NA
new$MAP2 <- as.numeric(levels(new$MAP2))[new$MAP2]
new$MAP <- ifelse(new$MAPraw == "watered", new$MAPmax, new$MAP2)

### Run P-model script to calculate GPP from coordinates
setwd("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/TBCFvsSOC")
source("scripts/Pmodel.R")
# Merge GPP with dat
gpp <- left_join(dplyr::select(dat, -GPP), unique(GPPextract[,c("LAT", "LONG", "GPP")]), by=c("LAT", "LONG"))
new <- left_join(dplyr::select(new, -GPP), dplyr::select(gpp,obs,GPP),)

### LAI ###
# https://land.copernicus.eu/global/products/fapar
#library(zip)
#library(plyr)
#dir <- "~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/LAI_2012/"
#filenames <- list.files(path=dir,pattern = "*.zip", full.names = TRUE, include.dirs=TRUE, recursive=TRUE)
#ldply(.data = filenames, .fun = unzip, exdir = dir)
#filenames <- list.files(path=dir,pattern = "*\\.tiff", full.names = TRUE, include.dirs=TRUE, recursive=TRUE)
#lai <- raster::stack(filenames)
#lai.mean <- raster::calc(lai, fun= mean, na.rm = T)
#writeRaster(lai.mean,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/LAImean_2012.tif",format="GTiff",overwrite=TRUE)
laimax <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/LAImax_2012.tif")
laimean <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/LAImean_2012.tif")
lai <- raster::stack(laimean,laimax)
names(lai) <- c("LAImean", "LAImax")
lai.df <- data.frame(obs=points$obs, raster::extract(lai,points))
new <- left_join(dplyr::select(new,-LAImax), lai.df)

### FPAR ###
# https://land.copernicus.eu/global/products/fapar
#library(zip)
#library(plyr)
#dir <- "~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/FPAR_2012/"
#filenames <- list.files(path=dir,pattern = "*.zip", full.names = TRUE, include.dirs=TRUE, recursive=TRUE)
#ldply(.data = filenames, .fun = unzip, exdir = dir)
#filenames <- list.files(path=dir,pattern = "*\\.tiff", full.names = TRUE, include.dirs=TRUE, recursive=TRUE)
#fpar <- raster::stack(filenames)
#fpar.mean <- raster::calc(fpar, fun= mean, na.rm = T)
#writeRaster(fpar.mean,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/FPARmean_2012.tif",format="GTiff",overwrite=TRUE)
fparmean <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/FPARmean_2012.tif")
fparmax <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/FPARmax_2012.tif")
fpar <- raster::stack(fparmean,fparmax)
names(fpar) <- c("fPARmean", "fPARmax")
fpar.df <- data.frame(obs=points$obs, raster::extract(fpar,points))
new <- left_join(new, fpar.df)

setwd("~/Documents/SoilC_CO2")
write.csv(new,"~/Downloads/extract.csv")
