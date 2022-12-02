
library(MODISTools)
library(ggpubr)
source('scripts/study_area.R')

# download day/night LST MODIS (1 km)
View(mt_products())
mt_bands(product = 'MOD11A2') %>% View() # bandas 7/8 -> day/nigh respectively
mt_dates(product = 'MOD11A2', lat = 40.414569, lon = -3.682567) %>% View()

# LST_Madrid_day <- mt_subset(product = 'MOD11A2',
#                         lat = 40.5671,
#                         lon =  -3.8387,
#                         band = c('LST_Day_1km','QC_Day'),
#                         start = '2017-01-01',
#                         end = '2021-12-31',
#                         km_lr = 150,
#                         km_ab = 150,
#                         site_name = 'Madrid',
#                         internal = TRUE,
#                         progress = TRUE)
# LST_Madrid_day$calendar_date <- LST_Madrid_day$calendar_date %>% as.Date(format='%Y-%m-%d')
# 
# LST_Madrid_night <- mt_subset(product = 'MOD11A2',
#                             lat = 40.5671,
#                             lon =  -3.8387,
#                             band = c('LST_Night_1km', 'QC_Night'),
#                             start = '2017-01-01',
#                             end = '2021-12-31',
#                             km_lr = 150,
#                             km_ab = 150,
#                             site_name = 'Madrid',
#                             internal = TRUE,
#                             progress = TRUE)
# LST_Madrid_night$calendar_date <- LST_Madrid_night$calendar_date %>% as.Date(format='%Y-%m-%d')
# 
# save(LST_Madrid_day, file='C:/Users/user/OneDrive/TESIS Y PUBLICACIONES/AIRTEC/madrid_urbantrees/results/LST_Madrid_day.Rdata')
# save(LST_Madrid_night, file='C:/Users/user/OneDrive/TESIS Y PUBLICACIONES/AIRTEC/madrid_urbantrees/results/LST_Madrid_night.Rdata')


# dates
# LST_Madrid_fechas <- mt_subset(product = 'MOD11A2',
#                                lat = 40.5671,
#                                lon =  -3.8387,
#                                band = c('Day_view_time','Night_view_time'),
#                                start = '2017-01-01',
#                                end = '2021-12-31',
#                                km_lr = 100,
#                                km_ab = 100,
#                                site_name = 'Madrid',
#                                internal = TRUE,
#                                progress = TRUE)
# LST_Madrid_fechas$calendar_date <- LST_Madrid_fechas$calendar_date %>% as.Date(format='%Y-%m-%d')
# 
# save(LST_Madrid_fechas, file='C:/Users/user/OneDrive/TESIS Y PUBLICACIONES/AIRTEC/madrid_urbantrees/results/LST_Madrid_fechas.Rdata')


par(mfrow=c(1,2), mar=c(5,5,5,5))
boxplot(subset(LST_Madrid_fechas,band=='Day_view_time')$value,
        subset(LST_Madrid_fechas,band=='Night_view_time')$value,
        xaxt = "n", main='Local solar time MODIS view')
axis(1, at=1:2, labels=c('Day','Night'))
mean(subset(LST_Madrid_fechas,band=='Day_view_time')$value)
mean(subset(LST_Madrid_fechas,band=='Night_view_time')$value)


# individual rasters
LST_day <- LST_Madrid_day %>% subset(band %in% 'LST_Day_1km')
LST_night <- LST_Madrid_night %>% subset(band %in% 'LST_Night_1km')
# change 0 to NAs
LST_day$value[LST_day$value==0] <- NA
LST_night$value[LST_night$value==0] <- NA


# quality check
hist(QC_day$value)
hist(QC_night$value)

# discard low quality values
QC_day <- LST_Madrid_day %>% subset(band=='QC_Day') %>% dplyr::select(calendar_date, pixel, value)
# QC_day$value[QC_day$value == 0 | QC_day$value == 17 | QC_day$value == 65] <- 1; QC_day$value[QC_day$value > 1 ] <- NA
QC_day$value <- 1
colnames(QC_day) <- c('calendar_date', 'pixel', 'Q2')
LST_Madrid_day_QC <- left_join(LST_day, QC_day, by=c('calendar_date','pixel'))
LST_Madrid_day_QC$value <- LST_Madrid_day_QC$value * LST_Madrid_day_QC$Q2
LST_Madrid_day_QC$Q2 <- NULL

QC_night <- LST_Madrid_night %>% subset(band=='QC_Night') %>% dplyr::select(calendar_date, pixel, value)
# QC_night$value[QC_night$value == 0 | QC_night$value == 17 | QC_night$value == 65] <- 1; QC_night$value[QC_night$value > 1 ] <- NA
QC_night$value <- 1
colnames(QC_night) <- c('calendar_date', 'pixel', 'Q2')
LST_Madrid_night_QC <- left_join(LST_night, QC_night, by=c('calendar_date','pixel'))
LST_Madrid_night_QC$value <- LST_Madrid_night_QC$value * LST_Madrid_night_QC$Q2
LST_Madrid_night_QC$Q2 <- NULL


# final dataframes
LST_Madrid_day_QC
LST_Madrid_night_QC


# exploratory
LST_Madrid_day_QC %>% subset(calendar_date > '2017-02-01' & calendar_date < '2017-04-30') %>% mt_to_raster(reproject = TRUE) %>% mean(na.rm=T) %>% plot()
LST_Madrid_night_QC %>% subset(calendar_date > '2020-02-01' & calendar_date < '2020-04-30') %>% mt_to_raster(reproject = TRUE) %>% mean(na.rm=T) %>% plot()



# seasonality of the UHI ####

vdates <- unique(LST_Madrid_day_QC$calendar_date)
df_mod_date <- matrix(nrow=length(vdates), ncol=3) %>% as.data.frame()
colnames(df_mod_date) <- c('date','LST_day','LST_night')
df_mod_date$date <- vdates

urban <- MAD_PATCH
rural <- buffer(urban, 15000) - urban
urban <- terra::project(urban, 'epsg:4326')
rural <- terra::project(rural, 'epsg:4326')

for (d in 1:nrow(df_mod_date)) {
  a <- LST_Madrid_day_QC %>% subset(calendar_date == df_mod_date$date[d]) %>% mt_to_raster(reproject = TRUE) %>% rast() %>% terra::crop(urban, mask=T) %>% as.vector() %>% mean(na.rm=T)
  b <- LST_Madrid_day_QC %>% subset(calendar_date == df_mod_date$date[d]) %>% mt_to_raster(reproject = TRUE) %>% rast() %>% terra::crop(rural, mask=T) %>% as.vector() %>% mean(na.rm=T)
  df_mod_date$LST_day[d] <- a - b
  
  a <- LST_Madrid_night_QC %>% subset(calendar_date == df_mod_date$date[d]) %>% mt_to_raster(reproject = TRUE) %>% rast() %>% terra::crop(urban, mask=T) %>% as.vector() %>% mean(na.rm=T)
  b <- LST_Madrid_night_QC %>% subset(calendar_date == df_mod_date$date[d]) %>% mt_to_raster(reproject = TRUE) %>% rast() %>% terra::crop(rural, mask=T) %>% as.vector() %>% mean(na.rm=T)
  df_mod_date$LST_night[d] <- a - b
  
  print(d/nrow(df_mod_date))
} 

# save
write.table(df_mod_date, 'results/df_mod_date.txt')
str(df_mod_date)


# plot
ggplot(aes(y=LST, x=date, group=passing, colour=passing), data=temp) +
  geom_point(shape=1) + theme_classic() +
  geom_smooth(method='loess', span=0.1) +
  labs(y=expression(paste(Delta, ' LST', sep=' ')), x=' ') +
  geom_hline(yintercept=c(0,mean(df_mod_date$LST_day),mean(df_mod_date$LST_night)), linetype=c('solid',"dashed","dashed"), color=c('black','#F8766D','#619CFF')) +
  geom_vline(xintercept= c(
    as.numeric(as.Date("2018-01-01", "%Y-%m-%d")),
    as.numeric(as.Date("2019-01-01", "%Y-%m-%d")),
    as.numeric(as.Date("2020-01-01", "%Y-%m-%d")),
    as.numeric(as.Date("2021-01-01", "%Y-%m-%d"))
  )
    , linetype=4, colour="black") + 
  theme(legend.position=c(0.65, 0.87), legend.title=element_blank(), legend.direction="horizontal")
  
ggarrange(a1,a2, nrow=1,ncol=2,widths=c(1,2))   
    


# rasters x season x day/nigth ####

LST_winter_17_day <- as( mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2016-12-21' & calendar_date < '2017-03-20'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_spring_17_day <- as( mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2017-03-20' & calendar_date < '2017-06-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_summer_17_day <- as( mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2017-06-21' & calendar_date < '2017-09-23'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_autumn_17_day <- as( mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2017-09-23' & calendar_date < '2017-12-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')

LST_winter_17_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2016-12-21' & calendar_date < '2017-03-20'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_spring_17_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2017-03-20' & calendar_date < '2017-06-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_summer_17_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2017-06-21' & calendar_date < '2017-09-23'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_autumn_17_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2017-09-23' & calendar_date < '2017-12-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')


LST_winter_18_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2017-12-21' & calendar_date < '2018-03-20'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_spring_18_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2018-03-20' & calendar_date < '2018-06-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_summer_18_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2018-06-21' & calendar_date < '2018-09-23'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_autumn_18_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2018-09-23' & calendar_date < '2018-12-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')

LST_winter_18_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2017-12-21' & calendar_date < '2018-03-20'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_spring_18_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2018-03-20' & calendar_date < '2018-06-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_summer_18_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2018-06-21' & calendar_date < '2018-09-23'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_autumn_18_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2018-09-23' & calendar_date < '2018-12-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')


LST_winter_19_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2018-12-21' & calendar_date < '2019-03-20'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_spring_19_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2019-03-20' & calendar_date < '2019-06-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_summer_19_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2019-06-21' & calendar_date < '2019-09-23'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_autumn_19_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2019-09-23' & calendar_date < '2019-12-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')

LST_winter_19_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2018-12-21' & calendar_date < '2019-03-20'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_spring_19_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2019-03-20' & calendar_date < '2019-06-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_summer_19_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2019-06-21' & calendar_date < '2019-09-23'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_autumn_19_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2019-09-23' & calendar_date < '2019-12-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')


LST_winter_20_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2019-12-21' & calendar_date < '2020-03-20'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_spring_20_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2020-03-20' & calendar_date < '2020-06-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_summer_20_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2020-06-21' & calendar_date < '2020-09-23'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_autumn_20_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2020-09-23' & calendar_date < '2020-12-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')

LST_winter_20_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2019-12-21' & calendar_date < '2020-03-20'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_spring_20_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2020-03-20' & calendar_date < '2020-06-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_summer_20_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2020-06-21' & calendar_date < '2020-09-23'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_autumn_20_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2020-09-23' & calendar_date < '2020-12-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')


LST_winter_21_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2020-12-21' & calendar_date < '2021-03-20'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_spring_21_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2021-03-20' & calendar_date < '2021-06-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_summer_21_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2021-06-21' & calendar_date < '2021-09-23'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_autumn_21_day <- as(  mt_to_raster( LST_Madrid_day_QC %>% subset(calendar_date > '2021-09-23' & calendar_date < '2021-12-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')

LST_winter_21_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2020-12-21' & calendar_date < '2021-03-20'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_spring_21_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2021-03-20' & calendar_date < '2021-06-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_summer_21_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2021-06-21' & calendar_date < '2021-09-23'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')
LST_autumn_21_night <- as(  mt_to_raster( LST_Madrid_night_QC %>% subset(calendar_date > '2021-09-23' & calendar_date < '2021-12-21'), reproject = TRUE) %>% mean(na.rm=T) - 273.15, 'SpatRaster')


# write rasters unproyected as geotiff

MODIS_rst <- list(LST_winter_17_day, LST_spring_17_day, LST_summer_17_day, LST_autumn_17_day, LST_winter_17_night, LST_spring_17_night, LST_summer_17_night, LST_autumn_17_night, LST_winter_18_day, LST_spring_18_day, LST_summer_18_day, LST_autumn_18_day, LST_winter_18_night, LST_spring_18_night, LST_summer_18_night, LST_autumn_18_night, LST_winter_19_day, LST_spring_19_day, LST_summer_19_day, LST_autumn_19_day, LST_winter_19_night, LST_spring_19_night, LST_summer_19_night, LST_autumn_19_night, LST_winter_20_day, LST_spring_20_day, LST_summer_20_day, LST_autumn_20_day, LST_winter_20_night, LST_spring_20_night, LST_summer_20_night, LST_autumn_20_night, LST_winter_21_day, LST_spring_21_day, LST_summer_21_day, LST_autumn_21_day, LST_winter_21_night, LST_spring_21_night, LST_summer_21_night, LST_autumn_21_night)

names(MODIS_rst) <- c('LST_winter_17_day','LST_spring_17_day','LST_summer_17_day','LST_autumn_17_day','LST_winter_17_night','LST_spring_17_night','LST_summer_17_night','LST_autumn_17_night','LST_winter_18_day','LST_spring_18_day','LST_summer_18_day','LST_autumn_18_day','LST_winter_18_night','LST_spring_18_night','LST_summer_18_night','LST_autumn_18_night','LST_winter_19_day','LST_spring_19_day','LST_summer_19_day','LST_autumn_19_day','LST_winter_19_night','LST_spring_19_night','LST_summer_19_night','LST_autumn_19_night','LST_winter_20_day','LST_spring_20_day','LST_summer_20_day','LST_autumn_20_day','LST_winter_20_night','LST_spring_20_night','LST_summer_20_night','LST_autumn_20_night',    'LST_winter_21_day','LST_spring_21_day','LST_summer_21_day','LST_autumn_21_day','LST_winter_21_night','LST_spring_21_night','LST_summer_21_night','LST_autumn_21_night')


for (i in 1:length(MODIS_rst)) {
  writeRaster(MODIS_rst[[i]], paste('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/', names(MODIS_rst)[i], '.tiff', sep=''))
  print(paste(round(i/32,0)*100,'% ---------', sep=''))
}


par(mfrow=c(2,4))
for (i in 1:length(MODIS_rst)) {
  plot(MODIS_rst[[i]], main=names(MODIS_rst)[i])
  lines(terra::project(MAD_PATCH,'epsg:4326'))
}


# nigth LST and urbanization
LST_night_mean <- terra::mean(LST_spring_17_night, LST_spring_18_night, LST_spring_19_night, LST_spring_20_night) %>% terra::project('epsg:32630')

ESM <- rast('C:/Users/user/Desktop/CAPAS_ROI/ESM.tif') %>% terra::resample(LST_night_mean, method='sum')

df_1 <- merge(as.data.frame(LST_night_mean, xy=T), as.data.frame(ESM, xy=T)); colnames(df_1) <- c("x","y","LST","ESM")

pp_1 <- vect(df_1, geom=c('x','y'), crs='epsg:32630')
DEM <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/DEM.tif') %>% terra::resample(LST_night_mean)
df_1$elevation <- terra::extract(DEM, pp_1)$Band_1

df_1 <- df_1 %>% subset(ESM > 1 & elevation > 600 & elevation < 700)
df_1$ESM <- df_1$ESM / max(df_1$ESM) * 100


ggplot(aes(y=LST, x=ESM), data=df_1) +
  geom_point() + theme_classic() +
  labs(x='Percentage of built-up area', y='Night LST Spring') +
  geom_smooth(method='lm')


lm(df_1$LST ~ df_1$ESM)
anova(lm(df_1$LST ~ df_1$ESM))
