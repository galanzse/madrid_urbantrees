
library(tidyverse)
library(terra)
library(mapSpain)

# Zhao cities nightlight dataset
ZHAO <- rast('C:/Users/user/Desktop/CAPAS_ROI/urbanextent_Zhao_2018.tif')

# download Madrid borders
MAD <- esp_get_ccaa(ccaa='Madrid') %>% vect() %>% project('epsg:4326')

# Madrid urban core from Zhao cities nightlight dataset
MAD_ZHAO <- ZHAO %>% terra::crop(MAD, mask=T)

# reproject to UTM
MAD <- MAD %>% project('epsg:32630')
MAD_ZHAO <- MAD_ZHAO %>% project('epsg:32630')

# create 1/0 raster
MAD_ZHAO[MAD_ZHAO==0] <- NA

# select biggest urban patch
MAD_PATCH <- terra::patches(MAD_ZHAO)
main_patch <- which.max(table(MAD_PATCH[MAD_PATCH])) %>% names() %>% as.numeric()
MAD_PATCH[MAD_PATCH!=main_patch] <- NA

# create polygon
MAD_PATCH <- terra::as.polygons(MAD_PATCH)

# 25 km buffer to have an entire gradient
STUDY_AREA <- terra::buffer(MAD_PATCH, 25000)

plot(MAD)
lines(MAD_PATCH, col='red')
lines(STUDY_AREA, col='blue')

