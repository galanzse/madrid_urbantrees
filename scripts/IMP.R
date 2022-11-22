
library(sf)
library(terra)
source('scripts/study_area.R')

# load reference grid and crop
imp_shp <- read_sf('E:/Imperviousness_2018/EEA grid Spain/es_100km.shp') %>% subset(EOFORIGIN > 2100000) %>% terra::vect() %>% terra::project('epsg:32630') %>% terra::crop(STUDY_AREA)
plot(imp_shp)

# identify useful cells
CellCode <- substring(imp_shp$CELLCODE, 6, 11)

# accedo a la carpeta de imagenes y hago un subset de las que necesito
filenames_imp <- list.files("E:/Imperviousness_2018/DATA", full.names=TRUE)
filenames_imp <- filenames_imp[substring(filenames_imp,61,63) == 'tif' & substring(filenames_imp,43,48) %in% CellCode]
filenames_imp <- filenames_imp[c(1,3,5)]

# import rasters
imp_l <- list()
for (i in 1:length(filenames_imp)) {
  imp_l[[i]] <- rast(filenames_imp[i])
  print(i)
}

# merge
imp_m <- terra::merge(imp_l[[1]], imp_l[[2]], imp_l[[3]])

# project and crop
IMP <- IMP %>% project('epsg:32630') %>% crop(STUDY_AREAM mask=T)

# check
plot(IMP); lines(STUDY_AREA)

# write
writeRaster(IMP, 'C:/Users/user/Desktop/CAPAS_ROI/IMP.tif', overwrite=TRUE)
