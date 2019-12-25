library(raster)

df <- read.csv("soilC - combined.csv",na.strings = "")
df$obs <- 1:nrow(df)
dat = df %>% filter(Experiment != "") %>% rename(biomass=yi) # Remove empty rows
dat$Latitude <- ifelse(dat$Lat %in% "S",dat$Latitude * -1,dat$Latitude)
dat$Longitude <- ifelse(dat$Lon %in% "W",dat$Longitude * -1,dat$Longitude)

points <- dplyr::select(dat,obs, Latitude,Longitude) %>% na.omit
coordinates(points) <- c("Longitude","Latitude")
projection(points) <- CRS ("+proj=longlat +ellps=WGS84")


### MAX MAP 1901-2017 ###
cruMAPmax <- raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/MAP.max.1901.2017.tif")
cruMAPmax.df <- data.frame(obs=points$obs, MAPmax=extract(cruMAPmax,points))
new <- left_join(df, cruMAPmax.df)
new$MAP2 <- new$MAPraw
new$MAP2[new$MAP2 == "watered"] <- NA
new$MAP2 <- as.numeric(levels(new$MAP2))[new$MAP2]
new$MAP <- ifelse(new$MAPraw == "watered", new$MAPmax, new$MAP2)

write.csv(new,"~/Downloads/extract.csv")
