
library(sf)
source('scripts/study_area.R')


# import STL
d_src <- 'E:/STL_Spain_2018/ES001L3_MADRID_UA2018_STL_v012/Data/ES001L3_MADRID_UA2018_STL_v012.gpkg'

# vectorize
STL <- st_read(d_src, layer='es001l3_MADRID_UA2018_STL') %>% terra::vect() %>% terra::project('epsg:32630')

# crop
STL <- STL %>% terra::crop(STUDY_AREA)


# import reference grid to rasterize
REF <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/ESM.tif')
REF[is.na(REF)] <- 0; REF[REF>0] <- 0


# rasterize
STL <- STL %>% terra::rasterize(REF)

# plot
plot(STL, col='black')


# write tiff
writeRaster(STL, 'C:/Users/user/Desktop/CAPAS_ROI/STL.tif', overwrite=TRUE)
