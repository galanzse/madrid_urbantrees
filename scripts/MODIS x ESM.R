
library(sf)
library(car)
library(GGally)
source('scripts/study_area.R')


# import data
ESM <- rast('C:/Users/user/Desktop/CAPAS_ROI/ESM.tif')
ESM[is.na(ESM)] <- 0
DEM <- rast('C:/Users/user/Desktop/CAPAS_ROI/DEM.tif')

modis_LST17_n <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_17_night.tiff') %>% terra::project('epsg:32630')
modis_LST18_n <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_18_night.tiff') %>% terra::project('epsg:32630')
modis_LST19_n <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_19_night.tiff') %>% terra::project('epsg:32630')
modis_LST20_n <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_20_night.tiff') %>% terra::project('epsg:32630')

modis_LST17_d <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_17_day.tiff') %>% terra::project('epsg:32630')
modis_LST18_d <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_18_day.tiff') %>% terra::project('epsg:32630')
modis_LST19_d <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_19_day.tiff') %>% terra::project('epsg:32630')
modis_LST20_d <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_20_day.tiff') %>% terra::project('epsg:32630')

MODIS_NIGHT <- (modis_LST17_n + modis_LST18_n + modis_LST19_n + modis_LST20_n) / 4
MODIS_NIGHT <- MODIS_NIGHT %>% terra::crop(STUDY_AREA, mask=T)

MODIS_DAY <- (modis_LST17_d + modis_LST18_d + modis_LST19_d + modis_LST20_d) / 4
MODIS_DAY <- MODIS_DAY %>% terra::crop(STUDY_AREA, mask=T)


# ESM ~ MODIS
# dataframe
ESM_rs <- terra::resample(x=ESM, y=MODIS_AVERAGE, method='sum')

ESMxMOD <- merge(terra::as.data.frame(ESM_rs, xy=T), terra::as.data.frame(MODIS_NIGHT, xy=T),
                 by=c('x','y'))
colnames(ESMxMOD) <- c("x","y","ESM","Night")

ESMxMOD <- merge(ESMxMOD, terra::as.data.frame(MODIS_DAY, xy=T),
                 by=c('x','y'))
colnames(ESMxMOD) <- c("x","y","ESM","Night","Day")

# add elevation data
ESMxMOD$ALT <- terra::extract(terra::aggregate(x=DEM, 100), vect(ESMxMOD, geom=c('x','y'), 'epsg:32630'))$Band_1
head(ESMxMOD)

# filter by altitude
ESMxMOD <- ESMxMOD %>% filter(ESM > 0 & ALT > 600 & ALT < 700)


# plots
par(mfrow=c(1,1), mar=c(5,5,5,5))
plot(Night ~ ESM, data=ESMxMOD)
abline(lm(Night ~ ESM, data=ESMxMOD), col='blue', lwd=2)
plot(Day ~ ESM, data=ESMxMOD)
abline(lm(Day ~ ESM, data=ESMxMOD), col='blue', lwd=2)


mod <- lm(Night ~ ESM + ALT + y, data=ESMxMOD)
summary(mod)
Anova(mod, type='II')
par(mfrow=c(2,2), mar=c(5,5,5,5)); plot(mod)


a1 <- ggplot(aes(x=ESM/max(ESM)*100,y=Night), data=ESMxMOD) +
  geom_point() +
  geom_smooth(method='lm', level=0.95) +
  labs(y='Night LST Spring (ºC) ', x='Percentage of built-up area') +
  theme_classic()

# plots from 'MODIS'

# df_mod_date <- read.csv("C:/Users/user/OneDrive/TESIS Y PUBLICACIONES/AIRTEC/madrid_urbantrees/results/df_mod_date.txt", sep="")
# temp2 <- df_mod_date %>% pivot_longer(cols=2:3, names_to='passing', values_to='LST')
# temp2$passing <- as.factor(temp2$passing); levels(temp2$passing) <- c('Day','Night')
# temp2$date <- as.Date(temp2$date, "%Y-%m-%d")
# a2 <- ggplot(aes(y=LST, x=date, group=passing, colour=passing), data=temp2) +
#   geom_point(shape=1) + theme_classic() +
#   geom_smooth(method='loess', span=0.1) +
#   labs(y=expression(paste(Delta, ' LST (ºC)', sep=' ')), x=' ') +
#   theme(legend.position=c(0.80,0.95), legend.title=element_blank(), legend.direction="horizontal") +
#   geom_hline(yintercept=c(0,mean(df_mod_date$LST_day),mean(df_mod_date$LST_night)), linetype=c('solid',"dashed","dashed"), color=c('black','#F8766D','#619CFF')) +
#   geom_vline(xintercept= c(
#     as.numeric(as.Date("2018-01-01", "%Y-%m-%d")),
#     as.numeric(as.Date("2019-01-01", "%Y-%m-%d")),
#     as.numeric(as.Date("2020-01-01", "%Y-%m-%d"))), linetype=4, colour="black") 
  
# ggarrange(a2,a1, labels=c('(a)','(b)'),
#           nrow=1,ncol=2,widths=c(1,1))   

