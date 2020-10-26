# 1. Sample random points from the global maps of the predictors
# 2. Plot an histogram with those values.
# 3. Plot on top an histogram of the actual values covered by the training dataset
# 4. Each histogram should be proportional to the relative importance of the predictor
# e.g. based on relative size from cowplot
source("1. Effect Size.R")
library(tidyverse)
library(raster)
library(cruts)
library(ncdf4)
library(cowplot)
make_pct <- function(x) (exp(x) - 1) * 100
antiperc <- function(x) log(x/100 + 1)
importance.values <- read.csv("VI_abs.csv") %>% arrange(desc(Overall)) %>%
  mutate(Overall=round(Overall,0)) %>% arrange(-Overall)

# Bplant
biomass <- raster("maps/Bbiomass.tif")
r<-init(biomass,"y")
rc <- reclassify(r, c(-90,-15,NA, -15,15,1, 15,60,NA,60,90,1))
biomass <- mask(biomass,rc,maskvalue=1)
biomass.df <- as.data.frame(biomass,xy=TRUE) %>% filter(!is.na(Bbiomass), Bbiomass !=0) %>% 
  sample_n(nrow(dat))
biomass.p<-ggplot(biomass.df, aes(x=make_pct(Bbiomass))) + 
  geom_histogram(aes(y=..density..), colour="#999999", fill="#999999",alpha=.2,bins = 30)+
  geom_density(data=dat,aes(x=make_pct(biomass)),alpha=.2, fill="#EFC000FF", col="#EFC000FF", size = 1) +
  xlab(expression(paste(CO[2]," effect on plant biomass (%)", sep=""))) +
  theme_cowplot()

# SOC
soc <- raster("~/OneDrive - LLNL/IIASA/maps/SOC_015cm_aggregated0p25_tha_SoilGrids2.tif") *100
r2<-init(soc,"y")
rc2 <- reclassify(r2, c(-90,-15,NA, -15,15,1, 15,60,NA,60,90,1))
soc <- mask(soc,rc2,maskvalue=1)
soc.df <- as.data.frame(soc,xy=TRUE) %>% filter(!is.na(SOC_015cm_aggregated0p25_tha_SoilGrids2)) %>%
  sample_n(nrow(dat))
soc.p<-ggplot(soc.df, aes(x=SOC_015cm_aggregated0p25_tha_SoilGrids2)) + 
  geom_histogram(aes(y=..density..), colour="#999999", fill="#999999",alpha=.2,bins = 30)+
  geom_density(data=dat,aes(x=amb),alpha=.2, fill="#EFC000FF", col="#EFC000FF", size = 1) +
  xlab(expression(paste("Soil C stocks  (g ", m^-2,")",sep="")))+
  theme_cowplot()

# Myc
data.myc <- filter(dat, Myc=="AM"| Myc=="ECM")
myc <- raster("maps/Myco_AMEM.tif")
myc <- mask(myc,rc,maskvalue=1)
myc.df <- as.data.frame(myc,xy=TRUE) %>% filter(!is.na(Myco_AMEM_category), Myco_AMEM_category != "NM") %>%
  sample_n(nrow(data.myc))
myc.p<-ggplot(myc.df, aes(x=Myco_AMEM_category)) + 
  stat_count(aes(y=..prop.., group=1),colour="#999999", fill="#999999",alpha=.2) +
  stat_count(data=data.myc, aes(y=..prop.., x=Myc, group=1),alpha=.2, fill="#EFC000FF", col="#EFC000FF", size = 1) +
  ylab("proportion") +
  xlab(expression(paste("Soil C stocks  (g ", m^-2,")",sep="")))+
    theme_cowplot()

# Fertilization
fert <- raster("~/OneDrive - LLNL/IIASA/maps/ESA_Nfertilization.tif")
fert <- mask(fert,rc,maskvalue=1)
fert.df <- as.data.frame(fert,xy=TRUE) %>% filter(!is.na(ESA_Nfertilization_category), ESA_Nfertilization_category != "") %>%
  sample_n(nrow(dat))
fert.p<-ggplot(fert.df, aes(x=ESA_Nfertilization_category)) + 
  stat_count(aes(y=..prop.., group=1),colour="#999999", fill="#999999",alpha=.2) +
  stat_count(data=dat, aes(y=..prop.., x=N, group=1),alpha=.2, fill="#EFC000FF", col="#EFC000FF", size = 1) +
  ylab("proportion") + xlab("Nutrient fertilization") +
  scale_x_discrete(labels = c('Yes','No'))+
  theme_cowplot()

# LAI
lai <- raster("~/OneDrive - LLNL/IIASA/maps/LAImax_2012_aggregated0p25.tif")
lai <- mask(lai,rc,maskvalue=1)
lai.df <- as.data.frame(lai,xy=TRUE) %>% filter(!is.na(LAImax_2012_aggregated0p25), LAImax_2012_aggregated0p25 !=0) %>%
  sample_n(nrow(dat))
lai.p<-ggplot(lai.df, aes(x=LAImax_2012_aggregated0p25)) + 
  geom_histogram(aes(y=..density..), colour="#999999", fill="#999999",alpha=.2,bins = 30)+
  geom_density(data=dat,aes(x=LAImax),alpha=.2, fill="#EFC000FF", col="#EFC000FF", size = 1) +
  xlab("Leaf Area Index")+
  theme_cowplot()

# Soil depth
depth.p<-ggplot() + 
  geom_density(data=dat, aes(x=Depth..cm., group=1),alpha=.2, fill="#EFC000FF", col="#EFC000FF", size = 1) +
  xlab("Soil depth (cm)")+
  theme_cowplot()

# MAP
prec <- cruts2raster("~/OneDrive - LLNL/IIASA/maps/cru_ts4.00.2011.2015.pre.dat.nc", timeRange=c("2012-01-01","2012-12-31")) # 2012; 
preSum <- sum (prec) # Sum precipitation
r3<-init(preSum,"y")
rc3 <- reclassify(r3, c(-90,-15,NA, -15,15,1, 15,60,NA,60,90,1))
preSum <- mask(preSum,rc3,maskvalue=1)
map.df <- as.data.frame(preSum,xy=TRUE) %>% filter(!is.na(layer)) %>%
  sample_n(nrow(dat))
map.p<-ggplot(map.df, aes(x=layer)) + 
  geom_histogram(aes(y=..density..,colour="covariate"), fill="#999999",alpha=.2,bins = 30)+
  scale_color_manual(name=element_blank(), values = c("covariate" = "#999999")) +
  geom_density(data=dat,aes(x=MAP, fill="training data"),alpha=.2, col="#EFC000FF", size = 1) +
  xlab("Mean Annual Precipitation (mm)")+
  scale_fill_manual(name = element_blank(), values = c("training data" = "#EFC000FF")) +
  theme_cowplot()

# MAT
temp <- cruts2raster("~/OneDrive - LLNL/IIASA/maps/cru_ts4.00.2011.2015.tmp.dat.nc", timeRange=c("2012-01-01","2012-12-31")) # 2012; 
tempMean <- mean (temp) # Average temperature
tempMean <- mask(tempMean,rc3,maskvalue=1)
mat.df <- as.data.frame(tempMean,xy=TRUE) %>% filter(!is.na(layer)) %>%
  sample_n(nrow(dat))
mat.p<-ggplot(mat.df, aes(x=layer)) + 
  geom_histogram(aes(y=..density..), colour="#999999", fill="#999999",alpha=.2,bins = 30)+
  geom_density(data=dat,aes(x=MAT),alpha=.2, fill="#EFC000FF", col="#EFC000FF", size = 1) +
  xlab("Mean Annual Temperature (°C)")+
  theme_cowplot()

## MULTIPLOT ##
# Legend
# extract the legend from one of the plots
legend <- get_legend(map.p)

lol <- plot_grid(biomass.p + labs(subtitle = paste("Importance = ",importance.values$Overall[1])) + theme(plot.subtitle = element_text(face = "bold",hjust = 0.5)), 
                 soc.p + labs(subtitle = paste("Importance = ",importance.values$Overall[2])) + theme(plot.subtitle = element_text(face = "bold",hjust = 0.5)), 
                 myc.p + labs(subtitle = paste("Importance = ",importance.values$Overall[3])) + theme(plot.subtitle = element_text(face = "bold",hjust = 0.5)), 
                 fert.p + labs(subtitle = paste("Importance = ",importance.values$Overall[4])) + theme(plot.subtitle = element_text(face = "bold",hjust = 0.5)), 
                 lai.p + labs(subtitle = paste("Importance = ",importance.values$Overall[5])) + theme(plot.subtitle = element_text(face = "bold",hjust = 0.5)),
                 depth.p + labs(subtitle = paste("Importance = ",importance.values$Overall[6])) + theme(plot.subtitle = element_text(face = "bold",hjust = 0.5)), 
                 map.p+ labs(subtitle = paste("Importance = ",importance.values$Overall[7])) + theme(legend.position="none", plot.subtitle = element_text(face = "bold",hjust = 0.5)),
                 mat.p + labs(subtitle = paste("Importance = ",importance.values$Overall[8])) + theme(plot.subtitle = element_text(face = "bold",hjust = 0.5)), 
                 NULL,
                 ncol=2, nrow=4, 
                 align="hv", rel_widths = c(1, 1, 1, 1, 1, 1))
final <-plot_grid(lol, legend, rel_widths = c(2, .4))
#final <- lol + draw_grob(legend, x=2/3, y=0, height = 0.3)
save_plot("graphs/uncertainties_hist.png", final, ncol=2, nrow=4, base_height = 3, base_width = 4, type = "cairo-png",dpi= 800,bg = "white")



