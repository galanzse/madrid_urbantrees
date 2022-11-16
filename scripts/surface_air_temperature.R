
library(ncdf4) 
library(rgdal) 
library(raster)

source('scripts/study_area.R')

MAD <- esp_get_munic(munic='Madrid', epsg="4326")[5,] %>% vect() %>% terra::project('epsg:32630')
MAD2 <- esp_get_ccaa(ccaa='Madrid', epsg="4326") %>% vect() %>% terra::project('epsg:32630')


# SAT Madrid April
nc_data_april <- nc_open('E:/dataset-sis-urban-climate-cities-88fb30af-cbd7-45b6-976a-ba858352ad2d/tas_Madrid_UrbClim_2017_04_v1.0.nc')
lon <- ncvar_get(nc_data_april, "longitude")
lat <- ncvar_get(nc_data_april, "latitude")
t <- ncvar_get(nc_data_april,"time")
tunits <- ncatt_get(nc_data_april,"time","units")
# nc to array
sat.array.april <- ncvar_get(nc_data_april, "tas") # array de sat
fillvalue <- ncatt_get(nc_data_april, "tas", "_FillValue") # los NAs estan codificados como 0s
nc_close(nc_data_april) # cierro el nc
# array
sat.array.april[sat.array.april == fillvalue$value] <- NA


# SAT Madrid May
nc_data_may <- nc_open('E:/dataset-sis-urban-climate-cities-88fb30af-cbd7-45b6-976a-ba858352ad2d/tas_Madrid_UrbClim_2017_05_v1.0.nc')
lon <- ncvar_get(nc_data_may, "longitude")
lat <- ncvar_get(nc_data_may, "latitude")
t <- ncvar_get(nc_data_may,"time")
tunits <- ncatt_get(nc_data_may,"time","units")
# nc to array
sat.array.may <- ncvar_get(nc_data_may, "tas") # array de sat
fillvalue <- ncatt_get(nc_data_may, "tas", "_FillValue") # los NAs estan codificados como 0s
nc_close(nc_data_may) # cierro el nc
# array
sat.array.may[sat.array.may == fillvalue$value] <- NA


# SAT de Madrid June
nc_data_june <- nc_open('E:/dataset-sis-urban-climate-cities-88fb30af-cbd7-45b6-976a-ba858352ad2d/tas_Madrid_UrbClim_2017_06_v1.0.nc')
lon <- ncvar_get(nc_data_june, "longitude")
lat <- ncvar_get(nc_data_june, "latitude")
t <- ncvar_get(nc_data_june,"time")
tunits <- ncatt_get(nc_data_june,"time","units")
# nc to array
sat.array.june <- ncvar_get(nc_data_june, "tas") # array de sat
fillvalue <- ncatt_get(nc_data_june, "tas", "_FillValue") # los NAs estan codificados como 0s
nc_close(nc_data_june) # cierro el nc
# array
sat.array.june[sat.array.june == fillvalue$value] <- NA


# SUHI at 11:00 AM (minimum intensity)
myseq <- c(11)
for (i in 1:29) { myseq <- c( myseq, tail(myseq, n=1) + 24) }

b1 <- list()
b2 <- list()
b3 <- list()
for (i in 1:length(myseq)) {
  sat.slice <- sat.array.april[, , myseq[i] ]
  b1[[i]] <- raster(t(sat.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) %>% rast()
  
  sat.slice <- sat.array.may[, , myseq[i] ]
  b2[[i]] <- raster(t(sat.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) %>% rast()
  
  sat.slice <- sat.array.june[, , myseq[i] ]
  b3[[i]] <- raster(t(sat.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) %>% rast()
}

b <- c( rast(b1), rast(b2), rast(b3) )
b <- mean(b, na.rm=T) - 273.15
b <- b %>% terra::project('epsg:32630')



# SUHI at 5:00 AM (maximum intensity)
myseq <- c(5)
for (i in 1:29) { myseq <- c( myseq, tail(myseq, n=1) + 24) }

d1 <- list()
d2 <- list()
d3 <- list()
for (i in 1:length(myseq)) {
  sat.slice <- sat.array.april[, , myseq[i] ]
  d1[[i]] <- raster(t(sat.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) %>% rast()
  
  sat.slice <- sat.array.may[, , myseq[i] ]
  d2[[i]] <- raster(t(sat.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) %>% rast()
  
  sat.slice <- sat.array.june[, , myseq[i] ]
  d3[[i]] <- raster(t(sat.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")) %>% rast()
}

d <- c( rast(d1), rast(d2), rast(d3) )
d <- mean(d, na.rm=T) - 273.15
d <- d %>% terra::project('epsg:32630')



# plots SAT + LST

modis_LST17 <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_17_night.tiff') %>% terra::project('epsg:32630')
modis_LST18 <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_18_night.tiff') %>% terra::project('epsg:32630')
modis_LST19 <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_19_night.tiff') %>% terra::project('epsg:32630')
modis_LST20 <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_20_night.tiff') %>% terra::project('epsg:32630')
modis_spring_night <- terra::mean(modis_LST17, modis_LST18, modis_LST19, modis_LST20)

modis_LST17 <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_17_day.tiff') %>% terra::project('epsg:32630')
modis_LST18 <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_18_day.tiff') %>% terra::project('epsg:32630')
modis_LST19 <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_19_day.tiff') %>% terra::project('epsg:32630')
modis_LST20 <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_20_day.tiff') %>% terra::project('epsg:32630')
modis_spring_day <- terra::mean(modis_LST17, modis_LST18, modis_LST19, modis_LST20)


par(mfrow=c(2,2))
pal <- colorRampPalette(c("white","red"))

plot(modis_spring_day, col = pal(10)); lines(nucleo); lines(MAD2); lines(as.polygons(ext(b)), lty=3)
plot(modis_spring_night, col = pal(10)); lines(nucleo); lines(MAD2); lines(as.polygons(ext(b)), lty=3)
plot(b, col = pal(10)); lines(nucleo)
plot(d, col = pal(10)); lines(nucleo)



# stats for paper
# 25 km buffer

RURAL <- buffer(MAD_PATCH, 25000) - MAD_PATCH

modis_spring_day %>% terra::crop(MAD_PATCH, mask=T) %>% as.vector() %>% mean(na.rm=T)
modis_spring_day %>% terra::crop(MAD_PATCH, mask=T) %>% as.vector() %>% sd(na.rm=T)
modis_spring_day %>% terra::crop(RURAL, mask=T) %>% as.vector() %>% mean(na.rm=T)
modis_spring_day %>% terra::crop(RURAL, mask=T) %>% as.vector() %>% sd(na.rm=T)

modis_spring_night %>% terra::crop(MAD_PATCH, mask=T) %>% as.vector() %>% mean(na.rm=T)
modis_spring_night %>% terra::crop(MAD_PATCH, mask=T) %>% as.vector() %>% sd(na.rm=T)
modis_spring_night %>% terra::crop(RURAL, mask=T) %>% as.vector() %>% mean(na.rm=T)
modis_spring_night %>% terra::crop(RURAL, mask=T) %>% as.vector() %>% sd(na.rm=T)


b %>% terra::crop(MAD_PATCH, mask=T) %>% as.vector() %>% mean(na.rm=T)
b %>% terra::crop(MAD_PATCH, mask=T) %>% as.vector() %>% sd(na.rm=T)
b %>% terra::crop(RURAL, mask=T) %>% as.vector() %>% mean(na.rm=T)
b %>% terra::crop(RURAL, mask=T) %>% as.vector() %>% sd(na.rm=T)

d %>% terra::crop(MAD_PATCH, mask=T) %>% as.vector() %>% mean(na.rm=T)
d %>% terra::crop(MAD_PATCH, mask=T) %>% as.vector() %>% sd(na.rm=T)
d %>% terra::crop(RURAL, mask=T) %>% as.vector() %>% mean(na.rm=T)
d %>% terra::crop(RURAL, mask=T) %>% as.vector() %>% sd(na.rm=T)


