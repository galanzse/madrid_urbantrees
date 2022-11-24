
source('scripts/study_area.R')

ROI35 <- ROI %>% project("epsg:3035")

# import DEM
dem1 <- terra::rast('E:/DEM_v11/eu_dem_v11_E30N20/eu_dem_v11_E30N20.TIF') %>% terra::crop(ROI35)
dem2 <- terra::rast('E:/DEM_v11/eu_dem_v11_E30N10/eu_dem_v11_E30N10.TIF') %>% terra::crop(ROI35)

# merge DEMs
DEM <- merge(dem1, dem2)

rm(dem1, dem2)

# reproject
DEM <- terra::project(x=DEM, y="epsg:32630")

# plot
plot(DEM); lines(ROI)

# write tiff
writeRaster(DEM, 'C:/Users/user/Desktop/CAPAS_ROI/DEM.tif', overwrite=TRUE)
