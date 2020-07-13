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

#### SOILGRIDS 2.0 ####
# https://files.isric.org/soilgrids/data/recent/ocs/
library(gdalUtils)
library(raster)
#newproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#link<-"/vsicurl/https://files.isric.org/soilgrids/data/recent/ocs/ocs_0-30cm_mean.vrt"
#soc<-raster(link)

###### 0-30cm #####
soc030 <- gdalwarp(tr=c(0.25,0.25), # Desired output resolution
         t_srs="EPSG:4326", multi=TRUE, 
         wm=200, output_Raster=TRUE,
co=c("BIGTIFF=IF_NEEDED", "COMPRESS=DEFLATE", "TILED=TRUE"), 
overwrite=TRUE,verbose=TRUE,
"/vsicurl/https://files.isric.org/soilgrids/data/recent/ocs/ocs_0-30cm_mean.vrt", # Input VRT
"soc_0-30cm_0p25_SoilGrids2.tif") # Output file

# SOILGRIDS 1.0
gdalwarp(tr=c(0.25,0.25), # Desired output resolution
         t_srs="EPSG:4326", multi=TRUE, 
         wm=200, output_Raster=TRUE,
         co=c("BIGTIFF=IF_NEEDED", "COMPRESS=DEFLATE", "TILED=TRUE"), 
         overwrite=TRUE,verbose=TRUE,
         #"/Users/terrermoreno1/Downloads/sol_organic.carbon.stock_msa.kgm2_m_250m_b0..30cm_2015_v0.2.tif", # Input
         "soc_0-30cm_0p25_SoilGrids1.tif") # Output file

#### Compare SoilGrids 1.0 vs 2.0 ###
#soc1<-raster("soc_0-30cm_0p25_SoilGrids1.tif") # kg/m2 
soc1<-raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_030cm_aggregated0p25_gm2.tif") # g/m2
soc2<-raster("soc_0-30cm_0p25_SoilGrids2.tif") # t/ha
#soc1<-projectRaster(soc1,soc2) * 10 # t/ha
soc1<-projectRaster(soc1,soc2) * 0.01 # t/ha
soc <- stack(soc2,-soc1)
soc.dif <- calc(soc, sum)
soc.dif[soc.dif< (-150)]<- -150
soc.dif_robin <- projectRaster(soc.dif,crs="+proj=robin",over=T)
dif.abs.df = as.data.frame(soc.dif_robin,xy=TRUE)

library(cptcity)
difcol <- cpt(pal = "ncl_BlueDarkRed18", rev=T, n=30)
#show_col(difcol)

p.dif.abs <- ggplot()+ 
  geom_polypath(data=oceanmap_df, aes(long,lat,group=group),fill= nord["nord5"], size = 0.1) +
  geom_raster(data=dif.abs.df,aes(x,y,fill=layer)) +
  geom_polypath(data=wmap_wgs_df, aes(long,lat,group=group),fill="transparent", color="black", size = 0.1) +
  geom_polygon(data=bbox_df, aes(x=long, y=lat), colour=nord["nord3"], fill="transparent", size = 0.3) +
  scale_fill_discrete_gradient(
    bins = 30,
    colours = difcol,
    #breaks=seq(-10,10,5),
    limits = c(-150, 150),
    guide = guide_colourbar(nbin = 100, raster = FALSE, frame.colour = "black", ticks.colour = NA)) +
  xlab(expression(paste("(SoilGrids 2.0 - SoilGrids 1.0) SOC stocks (Mg ", ha^-1,")",sep=""))) +
  coord_equal() + 
  theme_classic(base_size = 8) + 
  labs(subtitle="Red = lower SOC stocks in new version") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  theme_opts

save_plot("SoilGrids.png", p.dif.abs, dpi= 1200,type = "cairo-png",base_aspect_ratio = 1.5)

###### 0-15cm #####

# 0-5cm
c5<-gdalwarp(tr=c(0.25,0.25), # Desired output resolution
         t_srs="EPSG:4326", multi=TRUE, 
         wm=200, output_Raster=TRUE,
         co=c("BIGTIFF=IF_NEEDED", "COMPRESS=DEFLATE", "TILED=TRUE"), 
         overwrite=TRUE,verbose=TRUE,
         "/vsicurl/https://files.isric.org/soilgrids/data/recent/soc/soc_0-5cm_mean.vrt", # Input VRT
         "c5.tif") # dg/kg
c5<- c5*0.0001 #g/g
b5<-gdalwarp(tr=c(0.25,0.25), # Desired output resolution
         t_srs="EPSG:4326", multi=TRUE, 
         wm=200, output_Raster=TRUE,
         co=c("BIGTIFF=IF_NEEDED", "COMPRESS=DEFLATE", "TILED=TRUE"), 
         overwrite=TRUE,verbose=TRUE,
         "/vsicurl/https://files.isric.org/soilgrids/data/recent/bdod/bdod_0-5cm_mean.vrt", # Input VRT
         "b5.tif") # cg/cm3
b5<-b5*0.01 # Mg/m3
cfv5<-gdalwarp(tr=c(0.25,0.25), # Desired output resolution
             t_srs="EPSG:4326", multi=TRUE, 
             wm=200, output_Raster=TRUE,
             co=c("BIGTIFF=IF_NEEDED", "COMPRESS=DEFLATE", "TILED=TRUE"), 
             overwrite=TRUE,verbose=TRUE,
             "/vsicurl/https://files.isric.org/soilgrids/data/recent/cfvo/cfvo_0-5cm_mean.vrt", # Input VRT
             "cfv5.tif") # Output file
cfv5<-cfv5/1000 # cm3/cm3
soc5<- (c5*b5*0.05*(1-cfv5)) * 10000 # (g*Mg*m/g*m3 = Mg/m2) -> Mg/ha
cellStats(soc5,"mean", na.rm=T)
# 5-15cm
c15<-gdalwarp(tr=c(0.25,0.25), # Desired output resolution
             t_srs="EPSG:4326", multi=TRUE, 
             wm=200, output_Raster=TRUE,
             co=c("BIGTIFF=IF_NEEDED", "COMPRESS=DEFLATE", "TILED=TRUE"), 
             overwrite=TRUE,verbose=TRUE,
             "/vsicurl/https://files.isric.org/soilgrids/data/recent/soc/soc_5-15cm_mean.vrt", # Input VRT
             "c15.tif") # dg/kg
c15<- c15*0.0001 #g/g
b15<-gdalwarp(tr=c(0.25,0.25), # Desired output resolution
             t_srs="EPSG:4326", multi=TRUE, 
             wm=200, output_Raster=TRUE,
             co=c("BIGTIFF=IF_NEEDED", "COMPRESS=DEFLATE", "TILED=TRUE"), 
             overwrite=TRUE,verbose=TRUE,
             "/vsicurl/https://files.isric.org/soilgrids/data/recent/bdod/bdod_5-15cm_mean.vrt", # Input VRT
             "b15.tif") # cg/cm3
b15<-b15*0.01 # Mg/m3
cfv15<-gdalwarp(tr=c(0.25,0.25), # Desired output resolution
               t_srs="EPSG:4326", multi=TRUE, 
               wm=200, output_Raster=TRUE,
               co=c("BIGTIFF=IF_NEEDED", "COMPRESS=DEFLATE", "TILED=TRUE"), 
               overwrite=TRUE,verbose=TRUE,
               "/vsicurl/https://files.isric.org/soilgrids/data/recent/cfvo/cfvo_5-15cm_mean.vrt", # Input VRT
               "cfv15.tif") # Output file
cfv15<-cfv15/1000 # cm3/cm3
soc515<- (c15*b15*0.1*(1-cfv15)) * 10000 # (g*Mg*m/g*m3 = Mg/m2) -> Mg/ha
cellStats(soc515,"mean", na.rm=T)
# SOC 0-15cm
soc015<-soc5 + soc515
cellStats(soc015,"sum", na.rm=T)
cellStats(soc030,"sum", na.rm=T)
writeRaster(soc015,"~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_tha_SoilGrids2.tif")
cellStats(soc015,"mean", na.rm=T)
# Comparison with SoilGrids 1.0
soc1<-raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_gm2.tif") *0.01
soc1<- projectRaster(soc1,soc015)
cellStats(soc1,"sum", na.rm=T)

soc015SG<- stack(soc015,-soc1)
dif015<- calc(soc015SG,sum)
levelplot(dif015,at = do.breaks(c(-400,400),8),margin=FALSE,par.settings=RdBuTheme(region=(brewer.pal(8,'RdBu'))))

###### 0-15cm based on soil organic carbon densities #####
library(gdalUtils)
library(raster)
cd5<- gdalwarp(tr=c(0.25,0.25),
               t_srs="EPSG:4326", multi=TRUE, 
               wm=200, output_Raster=TRUE,
               co=c("BIGTIFF=IF_NEEDED", "COMPRESS=DEFLATE", "TILED=TRUE"), 
               overwrite=TRUE,verbose=TRUE,
               "/vsicurl/https://files.isric.org/soilgrids/data/recent/ocd/ocd_0-5cm_mean.vrt", # Input VRT
               "cd5.tif") # hg/dm³
cd515<- gdalwarp(tr=c(0.25,0.25), # Desired output resolution
                 t_srs="EPSG:4326", multi=TRUE, 
                 wm=200, output_Raster=TRUE,
                 co=c("BIGTIFF=IF_NEEDED", "COMPRESS=DEFLATE", "TILED=TRUE"), 
                 overwrite=TRUE,verbose=TRUE,
                 "/vsicurl/https://files.isric.org/soilgrids/data/recent/ocd/ocd_5-15cm_mean.vrt", # Input VRT
                 "cd515.tif") # hg/dm³

SOC15<- ((cd5*0.5) + (cd515*1) )/10
cellStats(SOC15,"sum", na.rm=T)
cellStats(soc030,"sum", na.rm=T)
# Comparison with SoilGrids 1.0
soc1<-raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_gm2.tif") *0.01
soc1<- projectRaster(soc1,SOC15)
cellStats(soc1,"sum", na.rm=T)
SOC15SG<- stack(SOC15,-soc1)
DIF15<- calc(SOC15SG,sum)
levelplot(DIF15,at = do.breaks(c(-400,400),8),margin=FALSE,par.settings=RdBuTheme(region=(brewer.pal(8,'RdBu'))))

SOC15_2<-raster("~/OneDrive/OneDrive - Universitat Autònoma de Barcelona/IIASA/maps/SOC_015cm_aggregated0p25_tha_SoilGrids2.tif")
DIF15_2<- SOC15 - SOC15_2
levelplot(DIF15,at = do.breaks(c(-400,400),8),margin=FALSE,par.settings=RdBuTheme(region=(brewer.pal(8,'RdBu'))))
