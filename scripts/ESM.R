
library(sf)
source('scripts/study_area.R')

# import European Settlement Map 10m
setwd('E:/ESM_2017')

# relevant layers: railways (2), BU open space 30,  BU streets(35), buildings (50)
# import and merge
ESM_reg_l <- list()
ESM_reg <- c('N20E30','N22E30') # grid cells of interest

for (i in 1:length(ESM_reg)) {
  
  myrast <- terra::rast(paste( ESM_reg[i], '/class_2/200km_10m_', ESM_reg[i], '_class2.TIF', sep = "")) +
            terra::rast(paste( ESM_reg[i], '/class_30/200km_10m_', ESM_reg[i], '_class30.TIF', sep = "")) +
            terra::rast(paste( ESM_reg[i], '/class_35/200km_10m_', ESM_reg[i], '_class35.TIF', sep = "")) +
            terra::rast(paste( ESM_reg[i], '/class_50/200km_10m_', ESM_reg[i], '_class50.TIF', sep = ""))
  
  # reduce values to 1/0
  myrast[myrast>0] <- 1; myrast[myrast==0] <- NA;
  
  # save raster
  ESM_reg_l[[i]] <- myrast
  
  # status
  print(paste(i,'out of',length(ESM_reg)))

}

# merge tiles
ESM_reg_m <- terra::merge(ESM_reg_l[[1]], ESM_reg_l[[2]])

# plot
plot(ESM_reg_m, col='red')

# crop study area
ROI35 <- ROI %>% project('epsg:3035')

ESM <- terra::crop(ESM_reg_m, ROI35, mask=T)

# reproject
ESM <- terra::project(ESM, y="epsg:32630")

# save tiff
writeRaster(ESM, 'C:/Users/user/Desktop/CAPAS_ROI/ESM.tif', overwrite=TRUE)




