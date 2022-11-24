
library(tidyverse)
library(terra)
library(mapSpain)

# Zhao cities nightlight dataset
ZHAO <- rast('C:/Users/user/Desktop/CAPAS_ROI/urbanextent_Zhao_2018.tif')

# Madrid
MAD <- esp_get_ccaa('madrid',epsg="4326") %>% vect()

# Madrid urban core from Zhao cities nightlight dataset
MAD_ZHAO <- ZHAO %>% terra::crop(MAD, mask=T)
MAD_ZHAO[MAD_ZHAO>0.9] <- 1; MAD_ZHAO[MAD_ZHAO<1] <- NA

# select biggest urban patch
MAD_PATCH <- terra::patches(MAD_ZHAO)
main_patch <- which.max(table(MAD_PATCH[MAD_PATCH])) %>% names() %>% as.numeric()
MAD_PATCH[MAD_PATCH!=main_patch] <- NA

# create polygon
MAD_PATCH <- terra::as.polygons(MAD_PATCH)

# 15 km buffer to have an entire gradient
STUDY_AREA <- terra::buffer(MAD_PATCH, 15000)

# reproject to UTM
MAD <- MAD %>% terra::project('epsg:32630')
MAD_ZHAO <- MAD_ZHAO %>% terra::project('epsg:32630')
MAD_PATCH <- MAD_PATCH %>% terra::project('epsg:32630')
STUDY_AREA <- STUDY_AREA %>% terra::project('epsg:32630')

# mask by altitude
DEM <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/DEM.tif') %>% terra::aggregate(10) %>%
  terra::crop(project(STUDY_AREA,'epsg:32630'), mask=T)
DEMstats <- DEM %>% terra::crop(MAD_PATCH, mask=T) %>% as.data.frame() %>% deframe() %>% boxplot.stats()
DEMmin <- DEMstats$stats[1]
DEMmax <- DEMstats$stats[5]

DEMmask <- DEM
DEMmask[DEMmask<DEMmin] <- NA; DEMmask[DEMmask>DEMmax] <- NA

# plot
par(mfrow=c(1,2))

plot(MAD_ZHAO, col='grey', legend=F)
lines(MAD)
lines(MAD_PATCH, col='red')
lines(STUDY_AREA, col='blue')

plot(DEMmask, legend=F)
lines(MAD)
lines(MAD_PATCH, col='black')
lines(STUDY_AREA, col='blue')

