
library(tidyverse)
library(terra)
library(climaemet)
library(chron)
library(ggpubr)
library(raster)
library(phenofit)
library(car)
library(MODISTools)


setwd('C:/Users/user/OneDrive/TESIS Y PUBLICACIONES/AIRTEC/madrid_urbantrees/data/climatic_stations')

load("C:/Users/user/OneDrive/TESIS Y PUBLICACIONES/AIRTEC/madrid_urbantrees/results/LST_Madrid_day.Rdata")
load("C:/Users/user/OneDrive/TESIS Y PUBLICACIONES/AIRTEC/madrid_urbantrees/results/LST_Madrid_night.Rdata")


# ground stations for validation
estaciones <- read.csv("estaciones_final.txt", sep="")

estaciones_points <- estaciones %>%
  vect(geom=c('x','y'), crs="epsg:4326") %>%
  project("epsg:32630")

# temperature series
temperature_series <- read.csv("C:/Users/user/OneDrive/TESIS Y PUBLICACIONES/AIRTEC/madrid_urbantrees/data/climatic_stations/data_merged.txt", sep="")
str(temperature_series)
temperature_series$FECHA <- as.Date(temperature_series$FECHA, "%Y-%m-%d")

# match stations and MODIS data
temperature_series <- temperature_series %>% filter(CODIGO %in% estaciones$CODIGO & FECHA %in% as.Date(LST_Madrid_day$calendar_date, "%Y-%m-%d"))

table(LST_Madrid_day$band)

# 0s to NAs
LST_Madrid_day$value[LST_Madrid_day$value==0] <- NA
LST_Madrid_night$value[LST_Madrid_night$value==0] <- NA

day_MODIS <- mt_to_raster(LST_Madrid_day[LST_Madrid_day$band=='LST_Day_1km',], reproject=TRUE)
night_MODIS <- mt_to_raster(LST_Madrid_night[LST_Madrid_night$band=='LST_Night_1km',], reproject=TRUE)


# LST data to temperature_series
temperature_series$LST <- NA

for (s in 1:nrow(temperature_series)) {
  
  ind <- temperature_series[s,'CODIGO'] %>% deframe()
  fec <- temperature_series[s,'FECHA'] %>% deframe()
  fec <- paste('X', gsub("-",".",fec), sep='')
  
  pp <- estaciones %>% filter(CODIGO==ind) %>% dplyr::select(x,y)
  
  temperature_series$LST[s] <- raster::extract(day_MODIS[[fec]], pp)
  
  print(round(s/nrow(temperature_series),2)*100)
}

LSTxESTACION <- na.omit(temperature_series)
LSTxESTACION <- LSTxESTACION %>% filter(LST > 0)

write.table(LSTxESTACION, 'LSTxESTACION.txt')


# plots LST ~ ground observations ###

LSTxESTACION$LST <- LSTxESTACION$LST - 273.15

a1 <- ggplot(aes(y=LST, x=min), data=LSTxESTACION) +
  xlim(0, 50) + ylim(0,50) +
  labs(x='Day Minimum Temperature Meteo. Station', y='MODIS Day Temperature') +
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  theme_classic()

a2 <- ggplot(aes(y=LST, x=mean), data=LSTxESTACION) +
  xlim(0, 50) + ylim(0,50) +
  labs(x='Day Mean Temperature Meteo. Station', y='MODIS Day Temperature') +
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  theme_classic()

a3 <- ggplot(aes(y=LST, x=max), data=LSTxESTACION) +
  xlim(0, 50) + ylim(0,50) +
  labs(x='Day Maximum Temperature Meteo. Station', y='MODIS Day Temperature') +
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  theme_classic()

ggarrange(a1,a2,a3, nrow=1, labels=c('(a)','(b)','(c)'), font.label=list(size=11))

# for (s in unique(LSTxESTACION$CODIGO)) {
#   plot(LST ~ mean, data=subset(LSTxESTACION, CODIGO==s), main=s); abline(0,1)
# }


# plots SUHI ~ urbanization

# LST
day_MODIS <- day_MODIS %>% terra::rast() %>% terra::project('epsg:32630')
night_MODIS <-  night_MODIS %>% terra::rast() %>% terra::project('epsg:32630')

# urbanization
ESM <- rast('C:/Users/user/Desktop/CAPAS_ROI/ESM.tif') %>% terra::resample(day_MODIS, method='sum')

# elevation
DEM <- rast('C:/Users/user/Desktop/CAPAS_ROI/DEM.tif') %>% terra::resample(day_MODIS)

# dataframe
df_pp <- matrix(nrow=dim(day_MODIS)[3], ncol=4) %>% as.data.frame()
colnames(df_pp) <- c('DATE','b_DAY_LST','b_NIGHT_LST','b_SUHI')
df_pp$DATE <- names(day_MODIS)

for (i in 1:nrow(df_pp)) {
  
  temp <- terra::as.data.frame(day_MODIS[df_pp$DATE[i]], xy=T); colnames(temp)[3] <- 'DAY'
  temp$NIGHT <- terra::extract(night_MODIS[df_pp$DATE[i]], vect(temp, geom=c('x','y'), 'epsg:32630'))[,2]
  temp$DIFF <- temp$DAY - temp$NIGHT
  temp$DEM <- terra::extract(DEM, vect(temp, geom=c('x','y'), 'epsg:32630'))$Band_1
  temp$ESM <- terra::extract(ESM, vect(temp, geom=c('x','y'), 'epsg:32630'))$Band_1
  
  temp <- temp %>% na.omit() %>% unique()
  
  df_pp$b_DAY_LST[i] <- lm(DAY ~ ESM + y + DEM, temp)$coefficients['ESM']
  df_pp$b_NIGHT_LST[i] <- lm(NIGHT ~ ESM + y + DEM, temp)$coefficients['ESM']
  df_pp$b_SUHI[i] <- lm(DIFF ~ ESM + y + DEM, temp)$coefficients['ESM']
  
  print(i/nrow(df_pp)*100)
  
}

df_pp$DATE <- df_pp$DATE %>% substr(2, 11) %>% as.Date("%Y.%m.%d")


ggplot(aes(x=DATE, y=b_SUHI), data=df_pp) +
  geom_point() +
  geom_smooth(span=0.3) +
  theme_classic()

a1 <- ggplot(aes(y=b_DAY_LST, x=DATE), data=df_pp) + geom_point() + 
  labs(y=expression(paste(beta, ' Day LST ~ Urbanization', sep=' ')), x='Date') +
  geom_smooth(method='loess', span=0.3) +
  ylim(-0.0004, 0.0006) +
  geom_vline(xintercept=as.numeric(as.Date("2018-01-01", "%Y-%m-%d")), linetype=4, colour="black") +
  geom_vline(xintercept=as.numeric(as.Date("2019-01-01", "%Y-%m-%d")), linetype=4, colour="black") +
  geom_vline(xintercept=as.numeric(as.Date("2020-01-01", "%Y-%m-%d")), linetype=4, colour="black") +
  geom_hline(yintercept=0, linetype=1, colour="black") + theme_classic()

a2 <- ggplot(aes(y=b_NIGHT_LST, x=DATE), data=df_pp) + geom_point() + 
  labs(y=expression(paste(beta, ' Night LST ~ Urbanization', sep=' ')), x='Date') +
  geom_smooth(method='loess', span=0.3) +
  ylim(-0.0004, 0.0006) +
  geom_vline(xintercept=as.numeric(as.Date("2018-01-01", "%Y-%m-%d")), linetype=4, colour="black") +
  geom_vline(xintercept=as.numeric(as.Date("2019-01-01", "%Y-%m-%d")), linetype=4, colour="black") +
  geom_vline(xintercept=as.numeric(as.Date("2020-01-01", "%Y-%m-%d")), linetype=4, colour="black") +
  geom_hline(yintercept=0, linetype=1, colour="black") + theme_classic()

a3 <- ggplot(aes(y=b_SUHI, x=DATE), data=df_pp) + geom_point() + 
  labs(y=expression(paste(beta, ' Difference ~ Urbanization', sep=' ')), x='Date') +
  geom_smooth(method='loess', span=0.3) +
  geom_vline(xintercept=as.numeric(as.Date("2018-01-01", "%Y-%m-%d")), linetype=4, colour="black") +
  geom_vline(xintercept=as.numeric(as.Date("2019-01-01", "%Y-%m-%d")), linetype=4, colour="black") +
  geom_vline(xintercept=as.numeric(as.Date("2020-01-01", "%Y-%m-%d")), linetype=4, colour="black") +
  geom_hline(yintercept=0, linetype=1, colour="black") + theme_classic()

ggarrange(a1, a2, a3, nrow=1, labels=c('(a)','(b)','(c)'))
