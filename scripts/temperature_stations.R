
library(tidyverse)
library(readxl)
library(ggrepel)
# install.packages('climaemet')
library(climaemet)


setwd('C:/Users/user/OneDrive/TESIS Y PUBLICACIONES/AIRTEC/Pollen_UrbanSources/data/climatic_stations')


# ESTACIONES ####

estaciones <- read_excel("estaciones.xlsx")
str(estaciones)

ggplot(estaciones, aes(x,y)) +
  geom_point()



# AYUNTAMIENTO DESDE 2019 ####

filenames <- list.files('Datos Ayuntamiento', full.names=TRUE)

data <- read.csv(filenames[1], sep=";")
for (i in 2:length(filenames)) {
  print(table(colnames(data)==colnames(read.csv(filenames[i], sep=";"))))
  data <- rbind(data, read.csv(filenames[i], sep=";"))
}

data <- data %>% subset(MAGNITUD==83)

v_horas <- c("H01","H02","H03","H04","H05","H06","H07","H08","H09","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23","H24")
v_check <- c("V01","V02","V03","V04","V05","V06","V07","V08","V09","V10","V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24")

data_ayto <- data[,1:8]
data_ayto$min <- NA
data_ayto$mean <- NA
data_ayto$max <- NA
data_ayto$H10 <- NA


temp <- data.frame(matrix(nrow=24, ncol=2))

for (i in 1:nrow(data_ayto)) {
  
  temp$temp <- data[i,v_horas] %>% unname() %>% t()
  temp$check <- data[i,v_check] %>% unname() %>% t()
  colnames(temp) <- c('temp','check')
  
  data_ayto$min[i] <- temp$temp[temp$check=='V'] %>% na.omit() %>% min()
  data_ayto$mean[i] <- temp$temp[temp$check=='V'] %>% na.omit() %>% mean()
  data_ayto$max[i] <- temp$temp[temp$check=='V'] %>% na.omit() %>% max()
  if (data[i,'V10']=='V') { data_ayto$H10[i] <- data[i,'H10'] }
  
  print(i)
  
}

data_ayto <- data_ayto %>% na.omit()

rm(data, i, f, filenames, v_check, v_horas, temp)

data_ayto$PROVINCIA <- NULL
data_ayto$MUNICIPIO <- NULL
data_ayto$MAGNITUD <- NULL
data_ayto$PUNTO_MUESTREO <- NULL

colnames(data_ayto)[colnames(data_ayto)=='ESTACION'] <- 'ID'

data_ayto <- merge(data_ayto, estaciones[,c('ID','CODIGO')], by='ID')

data_ayto$FECHA <- paste(data_ayto$ANO, '/', data_ayto$MES, '/', data_ayto$DIA,  sep='') %>% as.Date("%Y/%m/%d")
data_ayto$ANO <- NULL
data_ayto$MES <- NULL
data_ayto$DIA <- NULL

data_ayto$FUENTE <- 'AYTO'


# AEMET DESDE 2017 ####

apikey <- read.table('AEMET/API.txt')$V1

aemet_api_key(apikey, overwrite=TRUE, install=TRUE); aemet_detect_api_key()

estaciones_aemet <- subset(estaciones, FUENTE=='aemet')

data_aemet <- list()

for (s in 1:nrow(estaciones_aemet)) {
  data_aemet[[s]] <- aemet_daily_clim(estaciones_aemet$ID[s],
                                    start = "2019-01-01",
                                    end = "2020-12-31")
  print(estaciones_aemet$ID[s])
}

data_aemet <- data_aemet[1:13]
save(data_aemet[1:13], file="AEMET/rawdata.RData")


# RETENGO LAS TEMPERATURAS
for (s in 1:13) {
  data_aemet[[s]] <- data_aemet[[s]] %>% dplyr::select(fecha, indicativo, nombre, tmed, tmin, tmax)
}

data_aemet <- do.call(rbind, data_aemet)
data_aemet$H10 <- NA

colnames(data_aemet) <- c('FECHA', 'ID', 'CODIGO', 'mean', 'min', 'max', 'H10')

data_aemet$FUENTE <- 'AEMET'

data_aemet$CODIGO <- NULL
data_aemet <- merge(data_aemet, estaciones[,c('ID','CODIGO')], by='ID')

data_aemet <- data_aemet[,colnames(data_ayto)]



# UNO LAS TABLAS ####

data <- rbind(as.data.frame(data_ayto), as.data.frame(data_aemet))

str(data)

table(data$CODIGO)


# hago un summary por estacion
data <- data %>% group_by(CODIGO, FECHA, FUENTE) %>% summarise(min=mean(min, na.rm=T),
                                                               mean=mean(mean, na.rm=T),
                                                               max=mean(max, na.rm=T))


write.table(data, 'temperature_series_stations.txt')


