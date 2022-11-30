
library(sf)
source('scripts/study_area.R')

# project region of interest
STUDY_AREA30 <- STUDY_AREA %>% terra::project('epsg:3035')

LAEA_100grid <- terra::vect('E:/DLT_2018_010m_es_03035_v020/es_LAEA/ES_100K.shp') %>% terra::crop(STUDY_AREA30)
plot(LAEA_100grid)
CellCode <- unique(LAEA_100grid$CellCode)

# import DLT and mask by TCD and DEM
DLT_list <- list()
TCD_list <- list()
for (i in 1:length(CellCode)) {
  DLT_list[[i]] <- paste('E:/DLT_2018_010m_es_03035_v020/DATA/DLT_2018_010m_',
                   substr(CellCode[i],6,13), '_03035_v020.tif', sep='') %>% rast() %>% terra::crop(STUDY_AREA30, mask=T)
  TCD_list[[i]] <- paste('E:/TCD_2018_010m_es_03035_v020/DATA/TCD_2018_010m_',
                   substr(CellCode[i],6,13), '_03035_v020.tif', sep='') %>% rast() %>% terra::crop(STUDY_AREA30, mask=T)
}

# merge
DLT <- merge(DLT_list[[1]],DLT_list[[2]],DLT_list[[3]])
TCD <- merge(TCD_list[[1]],TCD_list[[2]],TCD_list[[3]])

# reproject
DLT <- terra::project(DLT, y="epsg:32630")
TCD <- terra::project(TCD, y="epsg:32630")

# save tiff
writeRaster(DLT, 'C:/Users/user/Desktop/CAPAS_ROI/DLT.tif', overwrite=TRUE)
writeRaster(TCD, 'C:/Users/user/Desktop/CAPAS_ROI/TCD.tif', overwrite=TRUE)
