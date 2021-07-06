                           #### loading the DMP 300 dataset VITO paltform in one step



rlistdmp300 <- list.files(pattern="DMP") 
rlistdmp300
import <- lapply(rlistdmp300, raster)
dmp300.multitemp <- stack(import)

# to crop images on my AOI (UPSP; 12°32'15"N lat. and 75°39'46"E long.)
ext <- c(75.25, 75.55, 12.15, 12.45)                                            
dmp300.up <- crop(dmp300.multitemp, ext)                                              # up= Uppangala, the Indian study area

names(dmp300.up) <- c("Jan 2014","Jan 2015","Jan 2016","Jan 2017","Jan 2018","Jan 2019","Jan 2020", "Jan 2021")
plot(dmp300.up)

boxplot(dmp300.up)

dmp300.up.resamp <- stretch(dmp300.up, minv=0, maxv=150)
plot(dmp300.up.resamp)

boxplot(dmp300.up.resamp)
boxplot(dmp300.up.resamp,outline=F, horizontal=T, axes=T, names=c("Jan 14", "Jan 15", "Jan 16","Jan 17","Jan 18","Jan 19", "Jan 20", "Jan 21"), main="Boxplot Dry Matter Productivity", col="gold")

                               #### loading the FaPAR 300 dataset VITO platform

setwd("D:/toy/FAPAR")

rlistFAPAR300 <- list.files(pattern="FAPAR") 
rlistFAPAR300
import <- lapply(rlistFAPAR300, raster)
FAPAR300.multitemp <- stack(import)

# to crop images on my AOI (UPSP; 12°32'15"N lat. and 75°39'46"E long.)
#ext <- c(75.10, 75.60, 12.05, 12.55)                                            
FAPAR300.up <- crop(FAPAR300.multitemp, ext)                                                        # up= Uppangala, the Indian study area

names(FAPAR300.up) <- c("Jan 2015","Jan 2016","Jan 2017","Jan 2018","Jan 2019","Jan 2020", "Jan 2021")
plot(FAPAR300.up)
boxplot(FAPAR300.up)



FAPAR300.up.resamp <- stretch(FAPAR300.up, minv=0, maxv=255)
FAPAR300.up.resamp.w <- reclassify(FAPAR300.up.resamp, cbind(253, 255, NA), right=TRUE)             #removing water pixels using cbind argument for 253, 255
levelplot(FAPAR300.up.resamp.w)                                                                     #plot the same image for factors=10 and factors=100
plot(FAPAR300.up.resamp.w)

boxplot(FAPAR300.up.resamp.w)
boxplot(FAPAR300.up.resamp.w,outline=F, horizontal=T, axes=T, names=c("Jan 15", "Jan 16","Jan 17","Jan 18","Jan 19", "Jan 20", "Jan 21"), main="Boxplot Fraction of Absorbed Photosynthetically Active Radiation ", col="gold")

                                 #### loading the NDVI 300 dataset VITO platform


setwd("D:/toy/NDVI")

rlistndvi300 <- list.files(pattern="NDVI") 
rlistndvi300
import <- lapply(rlistndvi300, raster)
ndvi300.multitemp <- stack(import)

# to crop images on my AOI (UPSP; 12°32'15"N lat. and 75°39'46"E long.)
#ext <- c(75.10, 75.60, 12.05, 12.55)                                            
ndvi300.up <- crop(ndvi300.multitemp, ext)                                              # up= Uppangala, the Indian study area

names(ndvi300.up) <- c("Jan 2014","Jan 2015","Jan 2016","Jan 2017","Jan 2018","Jan 2019","Jan 2020", "Jan 2021")
plot(ndvi300.up)
boxplot(ndvi300.up)

ndvi300.up.resamp <- stretch(dmp300.up, minv=0, maxv=255)
ndvi300.up.resamp.w <- reclassify(ndvi300.up.resamp, cbind(253, 255, NA), right=TRUE)             #removing water pixels using cbind argument for 253, 255
levelplot(ndvi300.up.resamp.w) 
plot(ndvi300.up.resamp.w)

boxplot(ndvi300.up.resamp.w)
boxplot(ndvi300.up.resamp.w,outline=F, horizontal=T, axes=T, names=c("Jan 14", "Jan 15", "Jan 16","Jan 17","Jan 18","Jan 19", "Jan 20", "Jan 21"), main="Normalized Difference Vegetation Index", col="gold")


                                ###### PCA and CORRELATION ANALYSIS


# correlation analysis of my images with ggpairs func. # require (GGally) 
#ggpairs(dmp300.up)
#ggpairs(FAPAR300.up)
#ggpairs(ndvi300.up)



# raster PCA to analyze all the dmp300.UP images data set to modelling all those objects in one single component(object)
dmpPCA <- rasterPCA(dmp300.up)
summary(dmpPCA$model)                                                          # PC1 model describes 93.7%
plot(dmpPCA$map)

# raster PCA to analyze all the FAPAR300.UP images data set to modelling all those objects in one single component(object)
faparPCA <- rasterPCA(FAPAR300.up)
summary(faparPCA$model)                                                          # PC1 model describes 93.7%
plot(faparPCA$map)

# raster PCA to analyze all the ndvi300.UP images data set to modelling all those objects in one single component(object)
ndviPCA <- rasterPCA(ndvi300.up)
summary(ndviPCA$model)                                                          # PC1 model describes 93.7%
plot(ndviPCA$map)

### correlation between indeces

#### dmp.up 21-14
setwd ("D:/toy/DMP")
dmp14 <- raster("c_gls_DMP300-RT5_202101100000_GLOBE_OLCI_V1.1.1.nc")
dmp21 <- raster("c_gls_DMP300-RT5_201401100000_GLOBE_PROBAV_V1.0.1.nc")

dmp14.up <- crop(dmp14, ext)
dmp21.up <- crop(dmp21, ext)

plot(dmp14.up, dmp21.up, col="blue", xlab="Dry Matter Productivity 2014 (Kg/ha/day)", ylab="Dry Matter Productivity 2021 (Kg/ha/day)")
abline(0,1,col="red")

## fapar.up 21-14
setwd ("D:/toy/FAPAR")
fapar15 <- raster("c_gls_FAPAR300_202101100000_GLOBE_OLCI_V1.1.1.nc")
fapar21 <- raster("c_gls_FAPAR300_201501100000_GLOBE_PROBAV_V1.0.1.nc")

fapar15.up <- crop(fapar15, ext)
fapar21.up <- crop(fapar21, ext)

plot(fapar15.up, fapar21.up, col="blue", xlab="Fraction Absorbed Photosynthetically Active Radiation 2015", ylab="Fraction Absorbed Photosynthetically Active Radiation 2021")
abline(0,1,col="red")

### ndvi.up 21-14
setwd ("D:/toy/DMP")
ndvi14 <- raster("c_gls_NDVI300_202101010000_GLOBE_OLCI_V2.0.1.nc")
ndvi21 <- raster("c_gls_NDVI300_201401010000_GLOBE_PROBAV_V1.0.1.nc")

ndvi14.up <- crop(ndvi14, ext)
ndvi21.up <- crop(ndvi21, ext)

plot(ndvi14.up, ndvi21.up, col="blue", xlab="Normalized Difference Vegetation Index 2014", ylab="Normalized Difference Vegetation Index 2021")
abline(0,1,col="red")

### Correlation analysis between different indeces

plot(fapar21.up, dmp21.up, col="blue")
abline(0,1,col="red")

plot(ndvi21.up, dmp21.up, col="green")
abline(0,1,col="red")

plot(fapar21.up, ndvi21.up, col="red")
abline(0,1,col="blue")


#ggpairs(dmpPCA, faparPCA, ndviPCA)

#abline
#plot(fapar.up, dmp.up, col="blue")
#plot(ndviPCA, dmpPCA, col="green")
#plot(faparPCA, ndviPCA, col="red")


                               ##### DIFFERENCE ANALYSIS


### Difference between 2021 - 2014 DMP

setwd("D:/toy/DMP")

   dmp21 <- raster ("c_gls_DMP300-RT5_202101100000_GLOBE_OLCI_V1.1.1.nc")
   dmp20 <- raster ("c_gls_DMP300-RT5_202001100000_GLOBE_PROBAV_V1.0.1.nc")
   dmp19 <- raster ("c_gls_DMP300-RT5_201901100000_GLOBE_PROBAV_V1.0.1.nc")
   dmp18 <- raster ("c_gls_DMP300-RT5_201801100000_GLOBE_PROBAV_V1.0.1.nc")
   dmp17 <- raster ("c_gls_DMP300-RT5_201701100000_GLOBE_PROBAV_V1.0.1.nc")
   dmp16 <- raster ("c_gls_DMP300-RT5_201601100000_GLOBE_PROBAV_V1.0.1.nc")
   dmp15 <- raster ("c_gls_DMP300-RT5_201501100000_GLOBE_PROBAV_V1.0.1.nc")
   dmp14 <- raster ("c_gls_DMP300-RT5_201401100000_GLOBE_PROBAV_V1.0.1.nc")

   dmp21.up <- crop(dmp21, ext)
   dmp20.up <- crop(dmp20, ext)
   dmp19.up <- crop(dmp19, ext)
   dmp18.up <- crop(dmp18, ext)
   dmp17.up <- crop(dmp17, ext)
   dmp16.up <- crop(dmp16, ext)
   dmp15.up <- crop(dmp15, ext)
   dmp14.up <- crop(dmp14, ext)
      
       dmp21.up.resamp <- stretch(dmp21.up, minv=0 , maxv=255)
       dmp20.up.resamp <- stretch(dmp20.up, minv=0 , maxv=255)
       dmp19.up.resamp <- stretch(dmp19.up, minv=0 , maxv=255)
       dmp18.up.resamp <- stretch(dmp18.up, minv=0 , maxv=255)
       dmp17.up.resamp <- stretch(dmp17.up, minv=0 , maxv=255)
       dmp16.up.resamp <- stretch(dmp16.up, minv=0 , maxv=255)
       dmp15.up.resamp <- stretch(dmp15.up, minv=0 , maxv=255)
       dmp14.up.resamp <- stretch(dmp14.up, minv=0 , maxv=255)


   dif.dmp.up21 <- dmp21.up - dmp14.up
   dif.dmp.up20 <- dmp20.up - dmp14.up
   dif.dmp.up19 <- dmp19.up - dmp14.up
   cl <- colorRampPalette(c("orange","white","light green","dark green")) (100)
   par(mfrow = c(2,1))
   plot(dif.dmp.up21, col= cl, main="Dry Matter Productivity Difference Jan'21 - Jan'14 ")
      hist(dif.dmp.up21, main="Histogram of Raster(DMP333m) Difference Jan'21- Jan'14")   

   plot(dif.dmp.up20, col= cl, main="Dry Matter Productivity Difference Jan'20 - Jan'14 ")
     hist(dif.dmp.up20, main="Histogram of Raster(DMP333m) Difference Jan'20- Jan'14")

   plot(dif.dmp.up19, col= cl, main="Dry Matter Productivity Difference Jan'19 - Jan'14 ")
      hist(dif.dmp.up19, main="Histogram of Raster(DMP333m) Difference Jan'19- Jan'14") 
   #ggR(dif.dmp.up, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Dry Matter Productivity Difference Jan'21 - Jan'14 ") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
   # histogram
   #par(mfrow = c(3,1))
   #hist(dif.dmp.up21, main="Histogram of Raster(DMP333m) Difference Jan'21- Jan'16") # breaks="16")
   #hist(dif.dmp.up20, main="Histogram of Raster(DMP333m) Difference Jan'20- Jan'16") 
   #hist(dif.dmp.up19, main="Histogram of Raster(DMP333m) Difference Jan'19- Jan'16") 

    # to detect the DMP trend year by year using boxplot function
    DMP <- stack(dmp14.up, dmp15.up, dmp16.up, dmp17.up, dmp18.up, dmp19.up, dmp20.up, dmp21.up)
    boxplot(DMP,outline=F, horizontal=T, axes=T, names=c("Jan 14", "Jan 15", "Jan 16","Jan 17","Jan 18","Jan 19", "Jan 20", "Jan 21"), main="Boxplot Dry Matter Productivity", col="gold")       # cancel the outliners 

    DMP.resamp <- stack(dmp14.up.resamp, dmp15.up.resamp, dmp16.up.resamp, dmp17.up.resamp, dmp18.up.resamp, dmp19.up.resamp, dmp20.up.resamp, dmp21.up.resamp)
    names(DMP.resamp) <- c("Jan 2014","Jan 2015","Jan 2016","Jan 2017","Jan 2018","Jan 2019","Jan 2020", "Jan 2021")
    levelplot(DMP.resamp)
    boxplot(DMP.resamp,outline=F, horizontal=T, axes=T, names=c("Jan 14", "Jan 15", "Jan 16","Jan 17","Jan 18","Jan 19", "Jan 20", "Jan 21"), col="gold") 

dev.off()


### Difference between 2021 - 2014 FAPAR

setwd("D:/toy/FAPAR")

   fapar21 <- raster ("c_gls_FAPAR300_202101100000_GLOBE_OLCI_V1.1.1.nc")
   fapar20 <- raster ("c_gls_FAPAR300_202001100000_GLOBE_PROBAV_V1.0.1.nc")
   fapar19 <- raster ("c_gls_FAPAR300_201901100000_GLOBE_PROBAV_V1.0.1.nc")
   fapar18 <- raster ("c_gls_FAPAR300_201801100000_GLOBE_PROBAV_V1.0.1.nc")
   fapar17 <- raster ("c_gls_FAPAR300_201701100000_GLOBE_PROBAV_V1.0.1.nc")
   fapar16 <- raster ("c_gls_FAPAR300_201601100000_GLOBE_PROBAV_V1.0.1.nc")
   fapar15 <- raster ("c_gls_FAPAR300_201501100000_GLOBE_PROBAV_V1.0.1.nc")
   
ext <- c(75.25, 75.55, 12.15, 12.45) 

   fapar21.up <- crop(fapar21, ext)
   fapar20.up <- crop(fapar20, ext)
   fapar19.up <- crop(fapar19, ext)
   fapar18.up <- crop(fapar18, ext)
   fapar17.up <- crop(fapar17, ext)
   fapar16.up <- crop(fapar16, ext)
   fapar15.up <- crop(fapar15, ext)
   
       fapar21.up.resamp <- stretch(fapar21.up, minv=0 , maxv=250)
       fapar20.up.resamp <- stretch(fapar20.up, minv=0 , maxv=250)
       fapar19.up.resamp <- stretch(fapar19.up, minv=0 , maxv=250)
       fapar18.up.resamp <- stretch(fapar18.up, minv=0 , maxv=250)
       fapar17.up.resamp <- stretch(fapar17.up, minv=0 , maxv=250)
       fapar16.up.resamp <- stretch(fapar16.up, minv=0 , maxv=250)
       fapar15.up.resamp <- stretch(fapar15.up, minv=0 , maxv=250)
       fapar14.up.resamp <- stretch(fapar14.up, minv=0 , maxv=250)


   dif.fapar.up21 <- fapar21.up - fapar15.up
   dif.fapar.up20 <- fapar20.up - fapar15.up
   dif.fapar.up19 <- fapar19.up - fapar15.up

par(mfrow = c(2,1))

   cl <- colorRampPalette(c("orange","white","light green","dark green")) (100)
   plot(dif.fapar.up21, col=cl , main ="FAPAR difference Jan'21 - Jan'15")
   hist(dif.fapar.up21, main="Histogram of Raster(FAPAR333m) Difference Jan'21- Jan'15")

     plot(dif.fapar.up20, col=cl , main ="FAPAR difference Jan'20 - Jan'15")
   hist(dif.fapar.up20, main="Histogram of Raster(FAPAR333m) Difference Jan'20- Jan'15")

      plot(dif.fapar.up19, col=cl , main ="FAPAR difference Jan'19 - Jan'15")
   hist(dif.fapar.up19, main="Histogram of Raster(FAPAR333m) Difference Jan'19- Jan'15")


   #plot(dif_fapar21.up, col= cl, main="Dry Matter Productivity Difference Jan'21 - Jan'14 ")
   #ggR(dif.fapar.up, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("FAPAR Difference Jan'21 - Jan'14 ") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
   

    # to detect the DMP trend year by year using boxplot function
    #FAPAR <- stack( fapar15.up, fapar16.up, fapar17.up, fapar18.up, fapar19.up, fapar20.up, fapar21.up)
    #boxplot(FAPAR,outline=F, horizontal=T, axes=T, names=c("Jan 15", "Jan 16","Jan 17","Jan 18","Jan 19", "Jan 20", "Jan 21"), main="Boxplot Fraction of Absorbed Photosynthetically Active Radiation", col="gold")       # cancel the outliners 

    FAPAR.resamp <- stack(fapar14.up.resamp, fapar15.up.resamp, fapar16.up.resamp, fapar17.up.resamp, fapar18.up.resamp, fapar19.up.resamp, fapar20.up.resamp, fapar21.up.resamp)
    names(FAPAR.resamp) <- c("Jan 2014","Jan 2015","Jan 2016","Jan 2017","Jan 2018","Jan 2019","Jan 2020", "Jan 2021")
    levelplot(FAPAR.resamp)
    boxplot(FAPAR.resamp,outline=F, horizontal=T, axes=T, names=c("Jan 14", "Jan 15", "Jan 16","Jan 17","Jan 18","Jan 19", "Jan 20", "Jan 21"), col="gold") 


### Difference between 2021 - 2014 NDVI 300

setwd("D:/toy/NDVI")

   ndvi21 <- raster ("c_gls_NDVI300_202101010000_GLOBE_OLCI_V2.0.1.nc")
   ndvi20 <- raster ("c_gls_NDVI300_202001010000_GLOBE_PROBAV_V1.0.1.nc")
   ndvi19 <- raster ("c_gls_NDVI300_201901010000_GLOBE_PROBAV_V1.0.1.nc")
   ndvi18 <- raster ("c_gls_NDVI300_201801010000_GLOBE_PROBAV_V1.0.1.nc")
   ndvi17 <- raster ("c_gls_NDVI300_201701010000_GLOBE_PROBAV_V1.0.1.nc")
   ndvi16 <- raster ("c_gls_NDVI300_201601010000_GLOBE_PROBAV_V1.0.1.nc")
   ndvi15 <- raster ("c_gls_NDVI300_201501010000_GLOBE_PROBAV_V1.0.1.nc")
   ndvi14 <- raster ("c_gls_NDVI300_201401010000_GLOBE_PROBAV_V1.0.1.nc")

   ndvi21.up <- crop(ndvi21, ext)
   ndvi20.up <- crop(ndvi20, ext)
   ndvi19.up <- crop(ndvi19, ext)
   ndvi18.up <- crop(ndvi18, ext)
   ndvi17.up <- crop(ndvi17, ext)
   ndvi16.up <- crop(ndvi16, ext)
   ndvi15.up <- crop(ndvi15, ext)
   ndvi14.up <- crop(ndvi14, ext)

   dif.ndvi21.up <- ndvi21.up - ndvi14.up
   dif.ndvi20.up <- ndvi20 - ndvi14.up
   dif.ndvi19.up <- ndvi19.up - ndvi14.up
   cl <- colorRampPalette(c("orange","white","light green","dark green")) (100)
   
   #plot(dif_dmp.up, col= cl, main="Dry Matter Productivity Difference Jan'21 - Jan'14 ")
   ggR(dif.ndvi.21up, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI Difference Jan'21 - Jan'14 ") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
   # histogramplot(dif.fapar.up19, col=cl , main ="FAPAR difference Jan'19 - Jan'15")

par(mfrow=c(1,2))

plot(dif.ndvi21.up, col=cl , main ="NDVI difference Jan'21 - Jan'14")
   hist(dif.ndvi21.up, main="Histogram of Raster(NDVI333m) Difference Jan'21- Jan'14")


plot(dif.ndvi20.up, col=cl , main ="NDVI difference Jan'20 - Jan'14")
   hist(dif.ndvi20.up, main="Histogram of Raster(NDVI333m) Difference Jan'20- Jan'14")


plot(dif.ndvi19.up, col=cl , main ="NDVI difference Jan'19 - Jan'14")
   hist(dif.ndvi19.up, main="Histogram of Raster(NDVI333m) Difference Jan'19- Jan'14")




    # to detect the DMP trend year by year using boxplot function
    NDVI <- stack(ndvi14.up, ndvi15.up, ndvi16.up, ndvi17.up, ndvi18.up, ndvi19.up, ndvi20.up, ndvi21.up)
    boxplot(NDVI,outline=F, horizontal=T, axes=T, names=c("Jan 14", "Jan 15", "Jan 16","Jan 17", "Jan 18", "Jan 19", "Jan 20", "Jan 21"), main="Boxplot Normalized Difference Vegetation Index", col="gold")       # cancel the outliners 




################################################ PROJECT SENTINEL 2 10M UPPANGALA RESERVE SHAPE VISUALIZATION

setwd("D:/toy/shape file  upsp foe Google eng/")

# Project: "crop the shape of Uppangala Reserve"
# Inserimento di uno shape.file e lettura di esso.

up.shp <- readOGR("D:/toy/shape file  upsp foe Google eng/usgs_uppangala.shp")     # shape di uppangala
summary (up.shp)
plot(up.shp)

###### Uppangala Permanent Plot

setwd("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP")

B <- readOGR("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP/Plot-B.shp") 
H <- readOGR("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP/Plot-H.shp") 
L <- readOGR("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP/Plot-L.shp") 
N <- readOGR("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP/Plot-N.shp") 
Q <- readOGR("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP/Plot-Q.shp") 
T <- readOGR("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP/Plot-T.shp") 

## loading the NDVI Sentinel 2 images for Uppangala Area and merging the images to cover the whole reserve

setwd("D:/toy/sentinel2ndviimages")

#2016
ndvi151224 <- raster("D:/toy/sentinel2ndviimages/ndvi-2016_01_03(1).tif")
ndvi151224a <- raster("D:/toy/sentinel2ndviimages/ndvi-2016_01_03.tif")
 q2016 <- merge(ndvi151224,ndvi151224a)
 #q2016b <- stretch(q2016a, minv=0, maxv=255)
 #storage.mode(q2016b[]) = "integer"
 #q2016 <- reclassify(q2016, cbind(253, 255, NA), right=TRUE) 

#2017
ndvi161228 <- raster("D:/toy/sentinel2ndviimages/ndvi-2016_12_28(1).tif")
ndvi161228a <- raster("D:/toy/sentinel2ndviimages/ndvi-2016_12_28.tif")
 q2017 <- merge(ndvi161228,ndvi161228a)
 
#2018
ndvi171228 <- raster("D:/toy/sentinel2ndviimages/ndvi-2017_12_28(1).tif")
ndvi171228a <- raster("D:/toy/sentinel2ndviimages/ndvi-2017_12_28.tif")
 q2018 <- merge(ndvi171228,ndvi171228a)

#2019
ndvi190107 <- raster("D:/toy/sentinel2ndviimages/ndvi-2019_01_07(1).tif")
ndvi190107a <- raster("D:/toy/sentinel2ndviimages/ndvi-2019_01_07.tif")
 q2019 <- merge(ndvi190107,ndvi190107a)
 
#2020
ndvi191228 <- raster("D:/toy/sentinel2ndviimages/ndvi-2020_01_12(1).tif")
ndvi191228a <- raster("D:/toy/sentinel2ndviimages/ndvi-2020_01_12.tif")
 q2020 <- merge(ndvi191228,ndvi191228a)

#2021
ndvi201227 <- raster("D:/toy/sentinel2ndviimages/ndvi-2020_12_27(1).tif")
ndvi201227a <- raster("D:/toy/sentinel2ndviimages/ndvi-2020_12_27.tif")
 q2021 <- merge(ndvi201227,ndvi201227a)

#par(mfrow = c(3,2))
 plot(q2016, main = "Jan 2016") # xlabel="Longitude in m from the local meridian", ylabel= "Latitude in m from the equator")
 plot(q2017, main = "Jan 2017")
 plot(q2018, main = "Jan 2018")
 plot(q2019, main = "Jan 2019")
 plot(q2020, main = "Jan 2020")
 plot(q2021, main = "Jan 2021")
 
dev.off()

  summary(q2016)
  summary(q2017)
  summary(q2018)
  summary(q2019)
  summary(q2020)
  summary(q2021)
   
    q2016
    q2017
    q2018
    q2019
    q2020
    q2021

##### plot with scico palette Uppangala quadrant multitemporal visualization
    
ggR(q2016, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2016") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(q2017, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2017") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(q2018, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2018") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(q2019, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2019") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(q2020, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2020") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(q2021, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2021") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))

par(mfrow = c(3,2))
hist(q2016, main = "Jan 2016")
hist(q2017, main = "Jan 2017")
hist(q2018, main = "Jan 2018")
hist(q2019, main = "Jan 2019")
hist(q2020, main = "Jan 2020")
hist(q2021, main = "Jan 2021")

NDVI10 <- stack(q2016, q2017, q2018, q2019, q2020, q2021)
boxplot(NDVI10,outline=F, horizontal=T, axes=T, names=c("Jan 16", "Jan 17", "Jan 18","Jan 19","Jan 20", "Jan 21"), main="Boxplot of Uppangala Quadrant NDVI10", col="gold")       # cancel the outliners 



##### difference and NDVI trends (Uppangala Area)

dif <- q2021- q2016
ggR(dif, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'21 - Jan'16") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(dif, main="Histogram of Raster(NDVI10m) Difference Jan'21 - Jan'16") #, breaks="16")
differ <- q2016-q2021
ggR(differ, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'16 - Jan'21") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(differ, main="Histogram of Raster(NDVI10m) Difference Jan'16 - Jan'21") #, breaks="16")

dif1 <- q2017 - q2016
ggR(dif1, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'17 - Jan'16") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(dif1, main="Histogram of Raster(NDVI10m) Difference Jan'17 - Jan'16") # breaks="16")

dif2 <- q2018 - q2017
ggR(dif2, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'18 - Jan'17") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(dif2, main="Histogram of Raster(NDVI10m) Difference Jan'18 - Jan'17") # breaks="16")

dif3 <- q2019 - q2018
ggR(dif3, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'19 - Jan'18") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(dif3, main="Histogram of Raster(NDVI10m) Difference Jan'19 - Jan'18") # breaks="16")

dif4 <- q2020 - q2019
ggR(dif4, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'20 - Jan'19") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(dif4, main="Histogram of Raster(NDVI10m) Difference Jan'20 - Jan'19")


dif5 <- q2021 - q2020
ggR(dif5, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'21 - Jan'20") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(dif5, main="Histogram of Raster(NDVI10m) Difference Jan'21 - Jan'20")

## boxplot difference ndvi 10m
NDVI10 <- stack(dif1, dif2, dif3, dif4, dif5)
boxplot(NDVI10,outline=F, horizontal=T, axes=T, names=c("Dif Jan 17/16", "Dif Jan 18-17", "Dif Jan 19-18","Dif Jan 20-19","Dif Jan 21-20"), main="Boxplot of Uppangala NDVI10 Multitemp differences", col="gold")       # cancel the outliners 



###### cropping the ndvi10 difference with uppangala reserve shape file

dup <- spTransform(up.shp, proj4string(dif))
duptot <- mask(crop(dif, extent(dup)), dup)

dup1 <- spTransform(up.shp, proj4string(dif1))
dup17 <- mask(crop(dif1, extent(dup1)), dup1)

dup2 <- spTransform(up.shp, proj4string(dif2))
dup18 <- mask(crop(dif2, extent(dup2)), dup2)

dup3 <- spTransform(up.shp, proj4string(dif3))
dup19 <- mask(crop(dif3, extent(dup3)), dup3)

dup4 <- spTransform(up.shp, proj4string(dif4))
dup20 <- mask(crop(dif4, extent(dup4)), dup4)

dup5 <- spTransform(up.shp, proj4string(dif5))
dup21 <- mask(crop(dif5, extent(dup5)), dup5)


ggR(duptot, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("difference Jan 21/16") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(dup17, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("difference Jan 17/16") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(dup18, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("difference Jan 18/17") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(dup19, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("difference Jan 19/18") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(dup20, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("difference Jan 20/19") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(dup21, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("difference Jan 21/20") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))

par(mfrow=c(3,2))
hist(duptot, main="NDVI Difference 21-16")
hist(dup17, main="NDVI Difference 17-16")
hist(dup18, main="NDVI Difference 18-17")
hist(dup19, main="NDVI Difference 19-18")
hist(dup20, main="NDVI Difference 20-19")
hist(dup21, main="NDVI Difference 21-20")

UPdif <- stack(duptot, dup17, dup18, dup19, dup20, dup21)
boxplot(UPNDVI10,outline=F, horizontal=T, axes=T, names=c("Jan 16", "Jan 17", "Jan 18","Jan 19","Jan 20", "Jan 21"), main="Boxplot of Uppangala Reserve Sentinel 2 NDVI10", col="gold")       # cancel the outliners 

############################################# cropping ndvi for uppangala shape file

up <- spTransform(up.shp, proj4string(q2016))
up16 <- mask(crop(q2016, extent(up)), up)

up1 <- spTransform(up.shp, proj4string(q2017))
up17 <- mask(crop(q2017, extent(up1)), up1)

up2 <- spTransform(up.shp, proj4string(q2018))
up18 <- mask(crop(q2018, extent(up2)), up2)

up3 <- spTransform(up.shp, proj4string(q2019))
up19 <- mask(crop(q2019, extent(up3)), up3)

up4 <- spTransform(up.shp, proj4string(q2020))
up20 <- mask(crop(q2020, extent(up4)), up4)

up5 <- spTransform(up.shp, proj4string(q2021))
up21 <- mask(crop(q2021, extent(up5)), up5)


##### data visualiztion for Uppangala shape (NDVI Sentinel2)

#par(mfrow = c(3,2))
plot(up16)
plot(up17)
plot(up18)
plot(up19)
plot(up20)
plot(up21)

### PLOTTING WITH SCICO NDVI10 MULTITEMP
ggR(up16, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2016") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(up17, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2017") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(up18, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2018") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(up19, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2019") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(up20, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2020") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(up21, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2021") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))

par( mfrow= c(3,2))
hist(up16, main = "Jan 2016")
hist(up17, main = "Jan 2017")
hist(up18, main = "Jan 2018")
hist(up19, main = "Jan 2019")
hist(up20, main = "Jan 2020")
hist(up21, main = "Jan 2021")

UPNDVI10 <- stack(up16, up17, up18, up19, up20, up21)
boxplot(UPNDVI10,outline=F, horizontal=T, axes=T, names=c("Jan 16", "Jan 17", "Jan 18","Jan 19","Jan 20", "Jan 21"), main="Boxplot of Uppangala Reserve Sentinel 2 NDVI10", col="gold")       # cancel the outliners 

hist(duptot, main="Histogram of Raster(NDVI10m) Difference Jan'21 - Jan'16")

dev off()


############################# make the same for the plot UP...

dif.B.tot <- spTransform(B, proj4string(dif))
db_tot <- mask(crop(dif, extent(dif.B.tot)), dif.B.tot)

dB1 <- spTransform(B, proj4string(dif1))
db_17 <- mask(crop(dif1, extent(dB1)), dB1)

dB2 <- spTransform(B, proj4string(dif2))
db_18 <- mask(crop(dif2, extent(dB2)), dB2)

dB3 <- spTransform(B, proj4string(dif3))
db_19 <- mask(crop(dif3, extent(dB3)), dB3)

dB4 <- spTransform(B, proj4string(dif4))
db_20 <- mask(crop(dif4, extent(dB4)), dB4)

dB5 <- spTransform(B, proj4string(dif5))
db_21 <- mask(crop(dif5, extent(dB5)), dB5)


########dataframe piero code
library(tidyverse)
library(raster)
library(RStoolbox)

fn <- system.file("external/test.grd", package="raster")

stc <- stack(
  fn,
  fn
)

stc_df <- fortify(dmp300.up, maxpixels = 5689958400) %>% 
  pivot_longer(
    .,
    cols = -(1:2),
    names_to = "layer",
  )

## arrange funtion: ordering the df in discending # asc goes by default x %>% arrange(variable)
stc_df %>%
   arrange(desc(value))

# if i want to filter the value for an year
stc_df %>%
   filter (layer== "Jan.2021") %>%
   arrange (desc(value))
 
#to mutate the DF. it is useful to add a new colomn
stc_df %>%
   filter (layer== "Jan.2019") %>%
   mutate(totvalue = 10201 * value )
   arrange (desc(value))

## data visualòization and scatter plot with ggplot2
## create a new dataset
dmp_values2021 <- stc_df %>%
   filter (layer== "Jan.2021")

# create a visualization in a scatterplot
library(ggplot2)

ggplot(stc_df, aes(x=layer, y= value)) +
       geom_point() # try to use also with x=, y=value and x=v y=y
       scale_x_long10()
       hist(dmp_values2021$value)
       ggplot(dmp_values2021, aes(x= x, y=value)) + geom_point()


## Analisi statistica e test parametrici
lineplot (NDVI, outline=F, horizontal=T, axes=T, )

plot(x, y1, type = "l")



## scatter plot of my df
plot(layer, value)
mod1 <- lm(bci_tur ~ bci_geogr)
mod1
abline(a = mod1$coefficients[1], b = mod1$coefficients[2], col = "red", lwd = 3)



########## to make a line in the scatter plot

plot(bci_geogr, bci_tur)
mod1 <- lm(bci_tur ~ bci_geogr)
mod1
abline(a = mod1$coefficients[1], b = mod1$coefficients[2], col = "red", lwd = 3)
