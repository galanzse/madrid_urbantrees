
library(tidyverse)
library(sf)
library(st)
library(terra)
library(ggfortify)
library(stars)
library(readxl)

# import DL raster
DLT <- rast('C:/Users/user/Desktop/CAPAS_ROI/DLT.tif')

# classification of broadleaf into evergreen and deciduous

# export random points to ID in google earth
# ss_STL <- df_STL[sample(1:nrow(df_STL), 2500),] %>% dplyr::select(x,y)
# ss_STL <- vect(ss_STL, geom=c('x','y'), 'epsg:32630') %>% project('epsg:4326') %>% geom() %>% as.data.frame() %>% select(x,y)
# write.csv(ss_STL, 'C:/Users/user/OneDrive/TESIS Y PUBLICACIONES/AIRTEC/Pollen_UrbanSources/data/ss_GE.csv', row.names = FALSE)

# import manually classified training layers
ss_GE <- read_excel("data/ss_GE.xlsx")
ss_GE <- ss_GE[!is.na(ss_GE$class),]

mykml <- st_read('data/polygons_v2.kml') %>% st_transform(32630)

k1 <- mykml[1,] %>% vect()
ss_buff <- terra::extract(DLT, k1, method="simple", xy=T)
ss_buff$class <- k1$Name
ss_buff <- ss_buff %>% dplyr::select(x,y,class) 
for (i in 1:dim(mykml)[1]) {
  try(k1 <- mykml[i,] %>% vect())
  k2 <- terra::extract(DLT, k1, xy=T)
  k2$class <- k1$Name
  k2 <- k2 %>% dplyr::select(x,y,class)  
  ss_buff <- rbind(ss_buff, k2)
}; rm(k1,k2)

# ss_buff <- na.omit(ss_buff)
table(ss_buff$class)

# merge tables
ss_GE <- ss_GE %>% dplyr::select(x,y,class)
ss_GE <- rbind(ss_buff, ss_GE)
ss_GE <- ss_GE %>% subset(class %in% c('deciduous','evergreen','golf','managed'))
rm(ss_buff)
# point file
pt_GE <- vect(ss_GE, geom=c('x','y'), 'epsg:32630')


# extract vegetation indexes
filenames_TUK <- list.files('E:/HR_VPP/TUK', full.names=T)
filenames_TUL <- list.files('E:/HR_VPP/TUL', full.names=T)
filenames_TVK <- list.files('E:/HR_VPP/TVK', full.names=T)
filenames_TVL <- list.files('E:/HR_VPP/TVL', full.names=T)

indxTUK <- filenames_TUK %>% rast(); names(indxTUK) <- c( paste( substring(filenames_TUK,44,50), substring(filenames_TUK,21,22), sep=''))
indxTUL <- filenames_TUL %>% rast(); names(indxTUL) <- c( paste( substring(filenames_TUL,44,50), substring(filenames_TUL,21,22), sep=''))
indxTVK <- filenames_TVK %>% rast(); names(indxTVK) <- c( paste( substring(filenames_TVK,44,50), substring(filenames_TVK,21,22), sep=''))
indxTVL <- filenames_TVL %>% rast(); names(indxTVL) <- c( paste( substring(filenames_TVL,44,50), substring(filenames_TVL,21,22), sep=''))

tempTUK <- terra::extract(indxTUK, pt_GE, method="simple", touches=T, xy=T) %>% as.data.frame()
tempTUK$class <- pt_GE$class
tempTUK <- tempTUK[-which(is.na(tempTUK$s1_SOSD17)),]
tempTUK <- tempTUK[which(is.na(tempTUK$s2_SOSD17) & is.na(tempTUK$s2_SOSD18) & is.na(tempTUK$s2_SOSD19) & is.na(tempTUK$s2_SOSD20) ),]
tempTUL <- terra::extract(indxTUL, pt_GE, method="simple", touches=T, xy=T) %>% as.data.frame()
tempTUL$class <- pt_GE$class
tempTUL <- tempTUL[-which(is.na(tempTUL$s1_SOSD17)),]
tempTUL <- tempTUL[which(is.na(tempTUL$s2_SOSD17) & is.na(tempTUL$s2_SOSD18) & is.na(tempTUL$s2_SOSD19) & is.na(tempTUL$s2_SOSD20) ),]
tempTVK <- terra::extract(indxTVK, pt_GE, method="simple", touches=T, xy=T) %>% as.data.frame()
tempTVK$class <- pt_GE$class
tempTVK <- tempTVK[-which(is.na(tempTVK$s1_SOSD17)),]
tempTVK <- tempTVK[which(is.na(tempTVK$s2_SOSD17) & is.na(tempTVK$s2_SOSD18) &  is.na(tempTVK$s2_SOSD19) &  is.na(tempTVK$s2_SOSD20) ),]
tempTVL <- terra::extract(indxTVL, pt_GE, method="simple", touches=T, xy=T) %>% as.data.frame()
tempTVL$class <- pt_GE$class
tempTVL <- tempTVL[-which(is.na(tempTVL$s1_SOSD17)),]
tempTVL <- tempTVL[which(is.na(tempTVL$s2_SOSD17) &  is.na(tempTVL$s2_SOSD18) & is.na(tempTVL$s2_SOSD19) & is.na(tempTVL$s2_SOSD20) ),]

# bind dataframes
df_GE <- rbind(tempTUK, tempTUL, tempTVK, tempTVL)
df_GE$ID <- NULL
df_GE$s2_SOSD17 <- NULL
df_GE$s2_SOSD18 <- NULL
df_GE$s2_SOSD19 <- NULL
df_GE$s2_SOSD20 <- NULL

# remove duplicate cells
df_GE <- df_GE %>% group_by(x,y) %>% sample_n(1)

# correct names
colnames(df_GE)[1:48] <- substr(colnames(df_GE), 4, 9)[1:48]

# compute amplitude 
df_GE$AMPL17 <- df_GE$MAXV17 - df_GE$MINV17
df_GE$AMPL18 <- df_GE$MAXV18 - df_GE$MINV18
df_GE$AMPL19 <- df_GE$MAXV19 - df_GE$MINV19
df_GE$AMPL20 <- df_GE$MAXV20 - df_GE$MINV20

# order columns 
df_GE <- df_GE[,order(colnames(df_GE))]

# fix dates
df_GE$SOSD17 <- df_GE$SOSD17 - 17000
df_GE$EOSD17 <- df_GE$EOSD17 - 17000
df_GE$MAXD17 <- df_GE$MAXD17 - 17000
df_GE$SOSD18 <- df_GE$SOSD18 - 18000
df_GE$EOSD18 <- df_GE$EOSD18 - 18000
df_GE$MAXD18 <- df_GE$MAXD18 - 18000
df_GE$SOSD19 <- df_GE$SOSD19 - 19000
df_GE$EOSD19 <- df_GE$EOSD19 - 19000
df_GE$MAXD19 <- df_GE$MAXD19 - 19000
df_GE$SOSD20 <- df_GE$SOSD20 - 20000
df_GE$EOSD20 <- df_GE$EOSD20 - 20000
df_GE$MAXD20 <- df_GE$MAXD20 - 20000

# remove outliers
df_GE$SOSD17 %>% hist()
df_GE <- df_GE[-which(df_GE$SOSD17 < 0 | df_GE$SOSD18 < 0 | df_GE$SOSD19 < 0 | df_GE$SOSD20 < 0),]
df_GE$EOSD17 %>% hist()
df_GE <- df_GE[-which(df_GE$EOSD17 > 400 | df_GE$EOSD18 > 400 | df_GE$EOSD19 > 400 | df_GE$EOSD20 > 400),]

# means for exploratory and classification
df_GE_med <- df_GE[,c('x','y','class')]
df_GE_med$EOSD <- df_GE[,c("EOSD17","EOSD18","EOSD19","EOSD20")] %>% rowMeans()
df_GE_med$EOSV <- df_GE[,c("EOSV17","EOSV18","EOSV19","EOSV20")] %>% rowMeans()
df_GE_med$LENG <- df_GE[,c("LENG17","LENG18","LENG19","LENG20")] %>% rowMeans()
df_GE_med$LSLO <- df_GE[,c("LSLO17","LSLO18","LSLO19","LSLO20")] %>% rowMeans()
df_GE_med$MAXD <- df_GE[,c("MAXD17","MAXD18","MAXD19","MAXD20")] %>% rowMeans()
df_GE_med$MAXV <- df_GE[,c("MAXV17","MAXV18","MAXV19","MAXV20")] %>% rowMeans()
df_GE_med$MINV <- df_GE[,c("MINV17","MINV18","MINV19","MINV20")] %>% rowMeans()
df_GE_med$RSLO <- df_GE[,c("RSLO17","RSLO18","RSLO19","RSLO20")] %>% rowMeans()
df_GE_med$SOSD <- df_GE[,c("SOSD17","SOSD18","SOSD19","SOSD20")] %>% rowMeans()
df_GE_med$SOSV <- df_GE[,c("SOSV17","SOSV18","SOSV19","SOSV20")] %>% rowMeans()
df_GE_med$SPRO <- df_GE[,c("SPRO17","SPRO18","SPRO19","SPRO20")] %>% rowMeans()
df_GE_med$TPRO <- df_GE[,c("TPRO17","TPRO18","TPRO19","TPRO20")] %>% rowMeans()
df_GE_med$AMPL <- df_GE[,c("AMPL17","AMPL18","AMPL19","AMPL20")] %>% rowMeans()

# save
write.table(df_GE, 'results/df_GE.txt')
write.table(df_GE_med, 'results/df_GE_med.txt')


# exploratory
table(df_GE_med$class)

# most interesting variables
myvar <- c("EOSD","EOSV","LENG","LSLO","MAXD","MAXV","MINV","RSLO","SOSD","SOSV","SPRO","TPRO","AMPL")

# correlation
pairs(df_GE_med[,myvar])

# differences among groups
long_GE_med <- df_GE_med %>% gather(4:16, key='trait', value='value')
ggplot(aes(x=class, y=value), data=long_GE_med) + geom_boxplot() + facet_wrap(~trait, scales = "free_y")

# visual inspection
out_ever <- df_GE_med[,c('x','y','class')]
out_ever <- vect(out_ever, geom=c('x','y'), 'epsg:32630') %>% project('epsg:4326') %>% terra::as.data.frame(geom='XY')
write.table(out_ever, 'results/out_ever.txt', row.names=F)

# correct points
df_GE_med$class[which(df_GE_med$class=='evergreen' & df_GE_med$LSLO>0.03)] <- 'deciduous'
df_GE_med$class[which(df_GE_med$class=='golf' & df_GE_med$EOSD<200)] <- 'managed'
df_GE_med$class[which(df_GE_med$class=='deciduous' & df_GE_med$SOSD<60)] <- NA
df_GE_med$class[which(df_GE_med$class=='evergreen' & df_GE_med$EOSD<200)] <- NA
df_GE_med$class[which(df_GE_med$class=='golf' & df_GE_med$MAXV<1.5)] <- NA
df_GE_med$class[which(df_GE_med$class=='deciduous' & df_GE_med$AMP<0.5)] <- NA
df_GE_med$class[which(df_GE_med$class=='evergreen' & df_GE_med$MAXV>1.75)] <- NA
df_GE_med$class[which(df_GE_med$class=='deciduous' & df_GE_med$LSLO<0.01)] <- NA

# remove NAs from means df
df_GE_med <- na.omit(df_GE_med)

# last inspection
long_GE_med <- df_GE_med %>% gather(4:16, key='trait', value='value')
ggplot(aes(x=class, y=value), data=long_GE_med) + geom_boxplot() + facet_wrap(~trait, scales = "free_y")

# add classes to original dataframe
df_GE <- inner_join(df_GE_med[,c('x','y','class')], df_GE, by=c('x','y'))

# save
write.table(df_GE, 'results/df_GE.txt')
write.table(df_GE_med, 'results/df_GE_med.txt')
