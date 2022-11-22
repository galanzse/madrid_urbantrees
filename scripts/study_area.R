
library(tidyverse)
library(terra)
library(mapSpain)

# Zhao cities nightlight dataset
ZHAO <- rast('C:/Users/user/Desktop/CAPAS_ROI/urbanextent_Zhao_2018.tif')

# download Madrid borders
MAD <- esp_get_ccaa(ccaa="madrid", epsg="4326") %>% vect()

# buffer in m to include nearby cities
MAD <- MAD %>% terra::project('epsg:32630') %>% buffer(width=50000) %>% terra::project('epsg:4326')

# Madrid urban core from Zhao cities nightlight dataset
MAD_ZHAO <- ZHAO %>% terra::crop(MAD, mask=T)

# reproject to UTM
MAD <- MAD %>% terra::project('epsg:32630')
MAD_ZHAO <- MAD_ZHAO %>% terra::project('epsg:32630')

# create 1/NA raster
MAD_ZHAO[MAD_ZHAO>0.9] <- 1
MAD_ZHAO[MAD_ZHAO<1] <- NA

# select biggest urban patch
MAD_PATCHES <- terra::patches(MAD_ZHAO)
# MAD_PATCH <- terra::patches(MAD_ZHAO)
# main_patch <- which.max(table(MAD_PATCH[MAD_PATCH])) %>% names() %>% as.numeric()
# MAD_PATCH[MAD_PATCH!=main_patch] <- NA

# create polygon
MAD_PATCHES <- terra::as.polygons(MAD_PATCHES)

# 25 km buffer to have an entire gradient
# STUDY_AREA <- terra::buffer(MAD_PATCH, 25000)

plot(MAD)
lines(MAD_PATCHES, col='red')
