# Satellite Vegetation Indices Trend (SVIT) is an Algorithm created by the Ph.D. student Baldo Marco to analyze the forest changes in terms of cover and productivity. 
# Together with PhD Piero Zannini, PhD student Kurian Ayushi and the Full Professor of the University of Bologna Duccio Rocchini, 
# We create an R script to analize the Dry Matter Productivity, Fraction of Absorbed Photosintetically Active Radiation and Normalized Different Vegetation Index trends of a selected area, in this case over the Western Ghats world Biodiversity Hotspot.
# We realized this analysis under the theory of the multispectral bands analysis and multiple time series analysis.
# Multiple Time Series Analysis on DMP, FAPAR, NDVI V2 1Km spatial scale from VITO catalogues 1999-2020 of the WG region

library(ncdf4)
library(raster)
library(RStoolbox)
library(rgdal)
library(gdalUtils)
library(ggplot2)
library(rasterVis)
library(GGally)
library(scico)

# List the products and loading all together
# Create the Dry Matter Productivity (DMP) Dataset 
# land.copernicus.vgt.vito.be (DMP 1km 1999-2020)

setwd("D:/uppangala/cop_dmp_1km/")                   
rlist <- list.files (pattern ="DMP")
rlist                                                
import <- lapply(rlist,raster)
dmp.multi <- stack(import) 

# Central Western Gaths Region = extention of the selected spatial grid (CSR : WGS84)
ext <- c(75.25, 75.55, 12.15, 12.45)                 

# DMP dataset
dmp.wg <- crop(dmp.multi,ext)

# Giving names at the products
names(dmp.wg) <- c("Jan 99"," Jan 00"," Jan 01"," Jan 02"," Jan 03","Jan 04","Jan 05","Jan 06","Jan 07","Jan 08","Jan 09","Jan 10","Jan 11","Jan 12","Jan 13","Jan 14","Jan 15","Jan 16","Jan 17","Jan 18","Jan 19","Jan 20")

# Correlation analyses amoung DMP dataset products of the single dataset
ggpairs(dmp.wg)

# DMP images multiple time serie
levelplot(dmp.wg, 
          names=c("Jan 99"," Jan 00"," Jan 01"," Jan 02"," Jan 03","Jan 04","Jan 05","Jan 06","Jan 07","Jan 08","Jan 09","Jan 10","Jan 11","Jan 12","Jan 13","Jan 14","Jan 15","Jan 16","Jan 17","Jan 18","Jan 19","Jan 20")
         )

# DMP multiple time series boxplot visualization
boxplot(dmp.wg, 
        outline=F, 
        horizontal=F, 
        axes=T, 
        names=c("Jan 99"," Jan 00"," Jan 01"," Jan 02"," Jan 03","Jan 04","Jan 05","Jan 06","Jan 07","Jan 08","Jan 09","Jan 10","Jan 11","Jan 12","Jan 13","Jan 14","Jan 15","Jan 16","Jan 17","Jan 18","Jan 19","Jan 20"), 
        col="gold"
       )


# Create the Fraction of Absorbed Photosinthetically Active Radiation (FAPAR) Dataset 
# land.copernicus.vgt.vito.be (FAPAR 1km 1999-2020)

setwd("D:/uppangala/cop_fapar_1km/")                   

# 22 years products
rlist2 <- list.files (pattern ="FAPAR")
rlist2                                              
import2 <- lapply(rlist2,raster)
fapar.multi <- stack(import2)

# FAPAR dataset
fapar.wg <- crop(fapar.multi,ext)

# Names the products
names(fapar.wg) <- c("Jan 99"," Jan 00"," Jan 01"," Jan 02"," Jan 03","Jan 04","Jan 05","Jan 06","Jan 07","Jan 08","Jan 09","Jan 10","Jan 11","Jan 12","Jan 13","Jan 14","Jan 15","Jan 16","Jan 17","Jan 18","Jan 19","Jan 20")

# Correlation analyses amoung products fo the FAPAR dataset
ggpairs(fapar.wg)

# FAPAR images multiple time serie
levelplot(fapar.wg, 
          names=c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
         )

# FAPAR multiple time series boxplot visualization
boxplot(fapar.wg, 
        outline=F, 
        horizontal=F, 
        axes=T, 
        names=c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"), 
        col="gold"
       )

# Create the Normalized Difference Vegetation Index (NDVI) Dataset 
# land.copernicus.vgt.vito.be (NDVI 1km 1999-2020)

setwd("D:/uppangala/cop_ndvi_1km/")                   

# 22 years
rlist3 <- list.files (pattern ="NDVI")
rlist3                                                 
import3 <- lapply(rlist3,raster)
ndvi.multi <- stack(import3)

# NDVI dataset
ndvi.wg <- crop(ndvi.multi,ext)

# Names the products
names(ndvi.wg) <- c("Jan 99"," Jan 00"," Jan 01"," Jan 02"," Jan 03","Jan 04","Jan 05","Jan 06","Jan 07","Jan 08","Jan 09","Jan 10","Jan 11","Jan 12","Jan 13","Jan 14","Jan 15","Jan 16","Jan 17","Jan 18","Jan 19","Jan 20")

# Correlation analyses amoung products fo the NDVI dataset
ggpairs(ndvi.wg)

# NDVI images multiple time serie
levelplot(ndvi.wg, names=c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"))

# NDVI multiple time series boxplot visualization
boxplot(ndvi.wg, 
        outline=F, 
        horizontal=F, 
        axes=T, 
        names=c("1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"), 
        col="gold"
       )


## Builds the three data frame

library(tidyverse)
#library(raster)
#library(RStoolbox)


#___________________DMP data frame_____________________

fn <- system.file("external/test.grd", package="raster")

stc <- stack(
  fn,
  fn
)

stc_df <- fortify(dmp.wg, maxpixels = 5689958400) %>% 
  pivot_longer(
    .,
    cols = -(1:2),
    names_to = "layer",
  )


#___________________Data frame structure_________________
stc_df$value
str(stc_df)


#___________________FAPAR data frame_____________________

fn <- system.file("external/test.grd", package="raster")

stc <- stack(
  fn,
  fn
)

stc_df2 <- fortify(fapar.wg, maxpixels = 5689958400) %>% 
  pivot_longer(
    .,
    cols = -(1:2),
    names_to = "layer",
  )

#___________________Data frame structure_________________
stc_df2

str(stc_df2)

stc_df$value


#___________________NDVI data frame_____________________

fn <- system.file("external/test.grd", package="raster")

stc <- stack(
  fn,
  fn
)

stc_df3 <- fortify(ndvi.wg, maxpixels = 5689958400) %>% 
  pivot_longer(
    .,
    cols = -(1:2),
    names_to = "layer",
  )

#___________________Data frame structure_________________
stc_df3$value

str(stc_df3)



#_____________Linear regression models___________________

#_______________________DMP______________________________

plot(stc_df)
mod1 <- lm(stc_df)
mod1
summary(mod1)
plot(mod1)
#abline(a = mod1$coefficients[1], b = mod1$coefficients[2], col = "red", lwd = 3)

#_______________________FAPAR___________________________

plot(stc_df2)
mod2 <- lm(stc_df2)
mod2
summary(mod2)
plot(mod2)
#abline(a = mod1$coefficients[1], b = mod1$coefficients[2], col = "red", lwd = 3)

#________________________NDVI___________________________

plot(stc_df3)
mod3 <- lm(stc_df3)
mod3
summary(mod3)
plot(mod3)
#abline(a = mod1$coefficients[1], b = mod1$coefficients[2], col = "red", lwd = 3)

# Computing the medians of each products of the datasets
# DMP
summary(dmp.wg)

# FAPAR
summary(fapar.wg)

# NDVI
summary(ndvi.wg)

# Principal Component Analysis (PCA) for raster objects
# PCA DMP
dmp_pca <- rasterPCA(dmp.wg)
plot(dmp_pca$map) 
summary(dmp_pca$model)

# PCA FAPAR
fapar_pca <- rasterPCA(fapar.wg)
plot(fapar_pca$map) 
summary(fapar_pca$model)

# PCA DMP
ndvi_pca <- rasterPCA(ndvi.wg)
plot(ndvi_pca$map) 
summary(ndvi_pca$model)


# Create Matrix for Medians and Time visualization
# DMP
summary(dmp.wg)
DMP_Medians <- c(93.19,79.89,79.18,81.87,77.41,79.43,83.84,83.86,94.74,88.61,92.42,94.85,81.11,87.39,75.16,80.39,100.36,88.80,96.69,84.68,109.67,85.39)

# FAPAR
summary(fapar.wg)
FAPAR_Medians <- c(0.736, 0.708, 0.68, 0.724, 0.692, 0.68, 0.728, 0.728, 0.728, 0.74, 0.756, 0.784, 0.74, 0.752, 0.736, 0.692, 0.804, 0.788, 0.736, 0.76, 0.82, 0.83)

# NDVI
summary(ndvi.wg)
NDVI_Medians <- c(0.752, 0.788, 0.76, 0.772, 0.704, 0.736, 0.752, 0.776, 0.74, 0.78, 0.776, 0.82, 0.756, 0.776, 0.788, 0.756, 0.816, 0.808, 0.74, 0.728, 0.816, 0.836)

# Time
Time <- c(1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

# Simple plot of DMP medians trend and distribution

plot(time,
     DMP_Medians, 
     type="l", 
     col="green", 
     lwd=5, xlab="time", 
     ylab="Kg / ha / day", 
     main="January Western Ghats Dry Matter Productivity"
    )

### Line fitting the DMP_Median values
### Line (DMP_Medians)
### Linear model (time ~ dmp medians)
m <- lm(time ~ DMP_Medians)
m
plot(time, DMP_Medians)

# Create a line fitting the points in the graph
abline(80.5205, 0.4439)
abline(lm(time1 ~ DMP_Medians))

# Quartiles plot
qqnorm(DMP_Medians, 
       pch = 1, 
       frame = FALSE
      )

# Create the Time ~ Medians data frame to use ggplot visualization for the 3 indices
# DMP
DMP_df <- data.frame(Time, DMP_Medians)
DMP_df

# Applicates Smooth Autoregressive Transition Model to the DMP Medians dataframe  
ggplot(DMP_df, 
       aes(Time, DMP_Medians)
      ) 
+ geom_point() 
+ stat_smooth()

# FAPAR
FAPAR_df <- data.frame(Time, FAPAR_Medians)
FAPAR_df

# Applicates Smooth Autoregressive Transition Model to the FAPAR Medians dataframe 
ggplot(DMP_df, 
       aes(Time, FAPAR_Medians)
      ) 
+ geom_point() 
+ stat_smooth()

# NDVI
NDVI_df <- data.frame(Time, NDVI_Medians)
NDVI_Medians

# Applicates Smooth Autoregressive Transition Model to the NDVI Medians dataframe 
ggplot(DMP_df, 
       aes(Time, NDVI_Medians)
      ) 
+ geom_point() 
+ stat_smooth()

#_____________________________________________________###### PCA and STATISTICAL ANALYSIS__________________________________________________________________________



# raster PCA analyzes all the DMP images products to modelling raster objects in a single one evaluating a single component (DMP index in this case)
dmpPCA <- rasterPCA(dmp.wg)
summary(dmpPCA$model)                                                          # PC1 model describes 93.7%
plot(dmpPCA$map)

# raster PCA analyzes all the FAPAR images products to modelling raster objects in a single one evaluating a single component (FAPAR index in this case)
faparPCA <- rasterPCA(fapar.wg)
summary(faparPCA$model)                                                         # PC1 model describes 93.7%
plot(faparPCA$map)

# raster PCA analyzes all the NDVI images products to modelling raster objects in a single one evaluating a single component (NDVI index in this case)
ndviPCA <- rasterPCA(ndvi.wg)
summary(ndviPCA$model)                                                          # PC1 model describes 93.7%
plot(ndviPCA$map)

#___________________________________________________________ Correlation between indeces

#### dmp.up 21-14
setwd ("D:/DMP")
dmp14 <- raster("c_gls_DMP300-RT5_202101100000_GLOBE_OLCI_V1.1.1.nc")
dmp21 <- raster("c_gls_DMP300-RT5_201401100000_GLOBE_PROBAV_V1.0.1.nc")

dmp14.up <- crop(dmp14, ext)
dmp21.up <- crop(dmp21, ext)

plot(dmp14.up, dmp21.up, col="blue", xlab="Dry Matter Productivity 2014 (Kg/ha/day)", ylab="Dry Matter Productivity 2021 (Kg/ha/day)")
abline(0,1,col="red")

## fapar.up 21-14
setwd ("D:/FAPAR")
fapar15 <- raster("c_gls_FAPAR300_202101100000_GLOBE_OLCI_V1.1.1.nc")
fapar21 <- raster("c_gls_FAPAR300_201501100000_GLOBE_PROBAV_V1.0.1.nc")

fapar15.up <- crop(fapar15, ext)
fapar21.up <- crop(fapar21, ext)

plot(fapar15.up, fapar21.up, col="blue", xlab="Fraction Absorbed Photosynthetically Active Radiation 2015", ylab="Fraction Absorbed Photosynthetically Active Radiation 2021")
abline(0,1,col="red")

### ndvi.up 21-14
setwd ("D:/NDVI")
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



                                           ##### DIFFERENTIAL ANALYSES #####

#_____________________________________PROJECT SENTINEL 2 10M UPPANGALA RESERVE SHAPE VISUALIZATION____________________________________________________________________

setwd("D:/reserve/")

# Project: "crop the shape of Uppangala Reserve"
# Inserts a shape file and reprojects it
# Kadamakal Reserve Forest and Pushpagiri Wildlife Sanctuary Shape File
up.shp <- readOGR("D:/reserve/shape file_uppangala.shp")
summary (up.shp)
plot(up.shp)

#_____________________________________Uppangala Permanent Plots Shape files______________________________________________________________________________________________________

setwd("D:/reserve/UPPANGALA_PLOTS_SHP")

B <- readOGR("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP/Plot-B.shp") 
H <- readOGR("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP/Plot-H.shp") 
L <- readOGR("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP/Plot-L.shp") 
N <- readOGR("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP/Plot-N.shp") 
Q <- readOGR("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP/Plot-Q.shp") 
T <- readOGR("D:/toy/shape file  upsp foe Google eng/UPPANGALA_PLOTS_SHP/Plot-T.shp") 

# Loads the NDVI Sentinel 2-L2A (10m) products related to the Indian Reserve
# Products have been downloaded from Google Engine thanks the related Python Script
# Merges the images to cover the whole reserve in one single quadrant per every year (the reserve falls under two different S2 products)
setwd("D:/Sentinel_2/")

# Create the single images covering the whole reserve borders in a quadrant.
# 2016 product
ndvi151224 <- raster("D:/toy/sentinel2ndviimages/ndvi-2016_01_03(1).tif")
ndvi151224a <- raster("D:/toy/sentinel2ndviimages/ndvi-2016_01_03.tif")
 q2016 <- merge(ndvi151224,ndvi151224a)
 #q2016b <- stretch(q2016a, minv=0, maxv=255)
 #storage.mode(q2016b[]) = "integer"
 #q2016 <- reclassify(q2016, cbind(253, 255, NA), right=TRUE) 

# 2017 product
ndvi161228 <- raster("D:/toy/sentinel2ndviimages/ndvi-2016_12_28(1).tif")
ndvi161228a <- raster("D:/toy/sentinel2ndviimages/ndvi-2016_12_28.tif")
 q2017 <- merge(ndvi161228,ndvi161228a)
 
# 2018 product
ndvi171228 <- raster("D:/toy/sentinel2ndviimages/ndvi-2017_12_28(1).tif")
ndvi171228a <- raster("D:/toy/sentinel2ndviimages/ndvi-2017_12_28.tif")
 q2018 <- merge(ndvi171228,ndvi171228a)

# 2019 product
ndvi190107 <- raster("D:/toy/sentinel2ndviimages/ndvi-2019_01_07(1).tif")
ndvi190107a <- raster("D:/toy/sentinel2ndviimages/ndvi-2019_01_07.tif")
 q2019 <- merge(ndvi190107,ndvi190107a)
 
# 2020 product
ndvi191228 <- raster("D:/toy/sentinel2ndviimages/ndvi-2020_01_12(1).tif")
ndvi191228a <- raster("D:/toy/sentinel2ndviimages/ndvi-2020_01_12.tif")
 q2020 <- merge(ndvi191228,ndvi191228a)

# 2021 product
ndvi201227 <- raster("D:/toy/sentinel2ndviimages/ndvi-2020_12_27(1).tif")
ndvi201227a <- raster("D:/toy/sentinel2ndviimages/ndvi-2020_12_27.tif")
 q2021 <- merge(ndvi201227,ndvi201227a)

# To visualize all the quadrants in a single image
 par(mfrow = c(3,2))
 plot(q2016, main = "Jan 2016") 
 plot(q2017, main = "Jan 2017")
 plot(q2018, main = "Jan 2018")
 plot(q2019, main = "Jan 2019")
 plot(q2020, main = "Jan 2020")
 plot(q2021, main = "Jan 2021")
 
dev.off()

# Information about statistics of the Sentinel 2 images merged
  summary(q2016)
  summary(q2017)
  summary(q2018)
  summary(q2019)
  summary(q2020)
  summary(q2021)

# Information about the dataset
    q2016
    q2017
    q2018
    q2019
    q2020
    q2021

##### Plot with scico palette Uppangala quadrant multitemporal visualization
    
ggR(q2016, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2016") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(q2017, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2017") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(q2018, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2018") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(q2019, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2019") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(q2020, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2020") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(q2021, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2021") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))

# To visualize all the quadrants in a single image
par(mfrow = c(3,2))
hist(q2016, main = "Jan 2016")
hist(q2017, main = "Jan 2017")
hist(q2018, main = "Jan 2018")
hist(q2019, main = "Jan 2019")
hist(q2020, main = "Jan 2020")
hist(q2021, main = "Jan 2021")

# Create a vector to be plotted in a bocplot
NDVI10 <- stack(q2016, q2017, q2018, q2019, q2020, q2021)

# Boxplot Visualization
boxplot(NDVI10,
        outline=F, 
        horizontal=T, 
        axes=T, 
        names=c("Jan 16", "Jan 17", "Jan 18","Jan 19","Jan 20", "Jan 21"), main="Boxplot of Uppangala Quadrant NDVI10", 
        col="gold"
       )        


#____________________________________Difference and NDVI trends (Uppangala Area)__________________________________________
#_____________________________________6 years Reserve NDVI differential Analyses__________________________________________

dif <- q2021- q2016

# Scintific Palette for Earth Land Changes Visualization
ggR(dif, geom_raster = TRUE) 
+ scale_fill_scico(palette = "romaO") 
+ ggtitle("NDVI difference Jan'21 - Jan'16") 
+ theme_light() 
+ theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))

# Histogram distribution of the differential 2021-2016 NDVI Sentinel 2 values pixels on the Kadamakal Reserve Forest borders
hist(dif, main="Histogram of Raster(NDVI10m) Difference Jan'21 - Jan'16") #, breaks="16")

# Longest Reserve NDVI difference (opposite)
differ <- q2016-q2021
ggR(differ, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'16 - Jan'21") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(differ, main="Histogram of Raster(NDVI10m) Difference Jan'16 - Jan'21") #, breaks="16")

# NDVI Differnce year by year and histogram distribution of the differential values pixels
# 2017-2016
dif1 <- q2017 - q2016
ggR(dif1, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'17 - Jan'16") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(dif1, main="Histogram of Raster(NDVI10m) Difference Jan'17 - Jan'16") # breaks="16")

# 2018 - 2017
dif2 <- q2018 - q2017
ggR(dif2, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'18 - Jan'17") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(dif2, main="Histogram of Raster(NDVI10m) Difference Jan'18 - Jan'17") # breaks="16")

# 2019 - 2018
dif3 <- q2019 - q2018
ggR(dif3, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'19 - Jan'18") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(dif3, main="Histogram of Raster(NDVI10m) Difference Jan'19 - Jan'18") # breaks="16")

# 2020 - 2019
dif4 <- q2020 - q2019
ggR(dif4, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'20 - Jan'19") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(dif4, main="Histogram of Raster(NDVI10m) Difference Jan'20 - Jan'19")

# 2021 - 2020
dif5 <- q2021 - q2020
ggR(dif5, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("NDVI difference Jan'21 - Jan'20") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
hist(dif5, main="Histogram of Raster NDVI 10m Difference Jan'21 - Jan'20")

## Boxplot difference ndvi 10m analyses
NDVI10 <- stack(dif1, dif2, dif3, dif4, dif5)

boxplot(NDVI10,
        outline=F, 
        horizontal=T, 
        axes=T, 
        names=c("Dif Jan 17/16", "Dif Jan 18-17", "Dif Jan 19-18","Dif Jan 20-19","Dif Jan 21-20"), 
        main="Boxplot of Uppangala NDVI10 Multitemp differences", 
        col="gold"
       )      



# Cropping the NDVI S2-L2A differences with Kadamakal Reserve Forest and Pushpagiri Wildlife Sanctuary Shape File (trakking borders)
# NDVI Total difference 2021-2016
dup <- spTransform(up.shp, proj4string(dif))
duptot <- mask(crop(dif, extent(dup)), dup)

# NDVI difference year by year
# 2017-2016
dup1 <- spTransform(up.shp, proj4string(dif1))
dup17 <- mask(crop(dif1, extent(dup1)), dup1)

# 2018-2017
dup2 <- spTransform(up.shp, proj4string(dif2))
dup18 <- mask(crop(dif2, extent(dup2)), dup2)

# 2019-2018
dup3 <- spTransform(up.shp, proj4string(dif3))
dup19 <- mask(crop(dif3, extent(dup3)), dup3)

# 2020-2019
dup4 <- spTransform(up.shp, proj4string(dif4))
dup20 <- mask(crop(dif4, extent(dup4)), dup4)

# 2021-2020
dup5 <- spTransform(up.shp, proj4string(dif5))
dup21 <- mask(crop(dif5, extent(dup5)), dup5)

# Total years difference in terms of NDVI on the Kadamakal Reserve Forest borders
ggR(duptot, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("difference Jan 21/16") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))

# Different Scico Visualization year by year on Kadamakal Reserve Forest borders
ggR(dup17, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("difference Jan 17/16") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(dup18, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("difference Jan 18/17") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(dup19, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("difference Jan 19/18") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(dup20, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("difference Jan 20/19") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(dup21, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("difference Jan 21/20") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))

# Visualization of the histograms of the raster difference analyses within the KRF border in one image
par(mfrow=c(3,2))
hist(duptot, main="NDVI Difference 21-16")
hist(dup17, main="NDVI Difference 17-16")
hist(dup18, main="NDVI Difference 18-17")
hist(dup19, main="NDVI Difference 19-18")
hist(dup20, main="NDVI Difference 20-19")
hist(dup21, main="NDVI Difference 21-20")

UPdif <- stack(duptot, dup17, dup18, dup19, dup20, dup21)
boxplot(UPNDVI10,outline=F, horizontal=T, axes=T, names=c("Jan 16", "Jan 17", "Jan 18","Jan 19","Jan 20", "Jan 21"), main="Boxplot of Uppangala Reserve Sentinel 2 NDVI10", col="gold")       # cancel the outliners 

# Crop the NDVI for Uppangala shapefile

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


# Visualization of the NDVI differential analyses in one image
par(mfrow = c(3,2))
plot(up16)
plot(up17)
plot(up18)
plot(up19)
plot(up20)
plot(up21)

### Plotting with scico the NDVI 10m products cropped for the KRF borders
ggR(up16, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2016") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(up17, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2017") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(up18, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2018") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(up19, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2019") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(up20, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2020") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))
ggR(up21, geom_raster = TRUE) + scale_fill_scico(palette = "romaO") + ggtitle("Jan 2021") + theme_light() + theme(plot.title.position ='plot', plot.title = element_text(hjust = 0.5))

# Visualization of the histograms KRF border in one image
par( mfrow= c(3,2))
hist(up16, main = "Jan 2016")
hist(up17, main = "Jan 2017")
hist(up18, main = "Jan 2018")
hist(up19, main = "Jan 2019")
hist(up20, main = "Jan 2020")
hist(up21, main = "Jan 2021")

# Boxplot of the KRF raster differential analyses
UPNDVI10 <- stack(up16, up17, up18, up19, up20, up21)
boxplot(UPNDVI10,
        outline=F, 
        horizontal=T, 
        axes=T, 
        names=c("Jan 16", "Jan 17", "Jan 18","Jan 19","Jan 20", "Jan 21"), 
        main="Boxplot of Uppangala Reserve Sentinel 2 NDVI10", 
        col="gold"
       )

# Histogram of the longest time differential analysis of the Sentinel 2 dataset related to the Kadamakal Reserve Forest Borders
hist(duptot, main="Histogram of Raster(NDVI10m) Difference Jan'21 - Jan'16")

dev off()


#_________________________________________Uppangala Plots Differential Analysis using the Uppanagala Sampling Plots Shape Files_____________________________

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


