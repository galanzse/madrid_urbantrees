
library(sf)
library(terra)
source('scripts/study_area.R')

# load reference grid and crop
imp_shp <- read_sf('E:/Imperviousness_2018/EEA grid Spain/es_100km.shp') %>% subset(EOFORIGIN > 2100000) %>% terra::vect() %>% terra::project('epsg:32630') %>% terra::crop(ROI_ZHAO)
plot(imp_shp)

# identify useful cells
CellCode <- substring(imp_shp$CELLCODE, 6, 11)

# accedo a la carpeta de imagenes y hago un subset de las que necesito
filenames_imp <- list.files("E:/Imperviousness_2018/DATA", full.names=TRUE)
filenames_imp <- filenames_imp[substring(filenames_imp,61,63) == 'tif' & substring(filenames_imp,43,48) %in% CellCode]
filenames_imp <- filenames_imp[c(1,3,5,7,9,11,13,15,17)]

# import rasters
imp_l <- list()
for (i in 1:length(filenames_imp)) {
  imp_l[[i]] <- rast(filenames_imp[i])
  print(i)
}

# merge
imp_m <- terra::merge(imp_l[[1]], imp_l[[2]], imp_l[[3]],
                      imp_l[[4]], imp_l[[5]], imp_l[[6]],
                      imp_l[[7]], imp_l[[8]], imp_l[[9]])

# project and crop
IMP <- imp_m %>% project('epsg:32630') %>% crop(ROI, mask=T)

# check
plot(IMP); lines(ROI)

# write
writeRaster(IMP, 'C:/Users/user/Desktop/CAPAS_ROI/IMP.tif', overwrite=TRUE)
