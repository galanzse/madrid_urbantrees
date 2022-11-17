
library(sf)
source('scripts/study_area.R')

# define my study area to crop rasters
ROI <- STUDY_AREA %>% terra::project('epsg:3035')

# find my grids of interest
LAEA_100grid <- terra::vect('E:/DLT_2018_010m_es_03035_v020/es_LAEA/ES_100K.shp') %>% terra::crop(ROI)
plot(LAEA_100grid)

# import tiles
r1 <- rast('E:/DLT_2018_010m_es_03035_v020/DATA/DLT_2018_010m_E31N19_03035_v020.tif') %>% terra::crop(ROI)
r2 <- rast('E:/DLT_2018_010m_es_03035_v020/DATA/DLT_2018_010m_E31N20_03035_v020.tif') %>% terra::crop(ROI)
r3 <- rast('E:/DLT_2018_010m_es_03035_v020/DATA/DLT_2018_010m_E32N20_03035_v020.tif') %>% terra::crop(ROI)

# merge
DLT <- terra::merge(r1, r2, r3)
DLT <- DLT %>% terra::crop(ROI, mask=T)

# project
DLT <- DLT %>% terra::project('epsg:32630')

# plot
plot(DLT); lines(MAD_PATCH)

# write tiff
writeRaster(DLT, 'C:/Users/user/Desktop/CAPAS_ROI/DLT.tif', overwrite=TRUE)


# HR-VPP data

# prepare dataframes
DLT <- rast('C:/Users/user/Desktop/CAPAS_ROI/DLT.tif')
DLT[!(DLT %in% c(1,2))] <- NA

# dataframe
df_DLT <- terra::as.data.frame(DLT, xy=T, cells=F, na.rm=T)
colnames(df_DLT)[colnames(df_DLT)=='AREA_KM2'] <- 'class'
pt_DLT <- vect(df_DLT[,c('x','y')], geom=c('x','y'), 'epsg:32630')

# mask DLT by DEM, and new point file
DEM <- rast('C:/Users/user/Desktop/CAPAS_ROI/DEM.tif')
df_DLT$elevation <- DEM %>% terra::extract(pt_DLT) %>% dplyr::select(Band_1) %>% deframe()
df_DLT <- df_DLT %>% filter(between(elevation, 600, 700))

# class as factor
df_DLT$class <- as.factor(df_DLT$class)
levels(df_DLT$class) <- c('broadleaf','coniferous')

# split coniferous and broadleaf
df_coniferous <- df_DLT %>% filter(class == 'coniferous')
df_broadleaf <- df_DLT %>% filter(class == 'broadleaf')
pt_coniferous <- vect(df_coniferous[,c('x','y')], geom=c('x','y'), 'epsg:32630') # new point file
pt_broadleaf <- vect(df_broadleaf[,c('x','y')], geom=c('x','y'), 'epsg:32630') # new point file

# list HR-VPP files
filenames_TUK <- list.files('E:/HR_VPP/TUK', full.names=T)
filenames_TUL <- list.files('E:/HR_VPP/TUL', full.names=T)
filenames_TVK <- list.files('E:/HR_VPP/TVK', full.names=T)
filenames_TVL <- list.files('E:/HR_VPP/TVL', full.names=T)
# load stacks
indxTUK <- filenames_TUK %>% rast(); names(indxTUK) <- c( paste( substring(filenames_TUK,44,50), substring(filenames_TUK,21,22), sep=''))
indxTUL <- filenames_TUL %>% rast(); names(indxTUL) <- c( paste( substring(filenames_TUL,44,50), substring(filenames_TUL,21,22), sep=''))
indxTVK <- filenames_TVK %>% rast(); names(indxTVK) <- c( paste( substring(filenames_TVK,44,50), substring(filenames_TVK,21,22), sep=''))
indxTVL <- filenames_TVL %>% rast(); names(indxTVL) <- c( paste( substring(filenames_TVL,44,50), substring(filenames_TVL,21,22), sep=''))


# coniferous ####

# extract values -> remove empty observations -> retain pixels with 1 season every year
tempTUK <- terra::extract(indxTUK, pt_coniferous, xy=T) %>% as.data.frame()
tempTUK <- tempTUK[-which(is.na(tempTUK$s1_SOSD17)),]
tempTUK <- tempTUK[which(is.na(tempTUK$s2_SOSD17) & 
                         is.na(tempTUK$s2_SOSD18) & 
                         is.na(tempTUK$s2_SOSD19) & 
                         is.na(tempTUK$s2_SOSD20) ),]

tempTUL <- terra::extract(indxTUL, pt_coniferous, xy=T) %>% as.data.frame()
tempTUL <- tempTUL[-which(is.na(tempTUL$s1_SOSD17)),]
tempTUL <- tempTUL[which(is.na(tempTUL$s2_SOSD17) & 
                           is.na(tempTUL$s2_SOSD18) & 
                           is.na(tempTUL$s2_SOSD19) & 
                           is.na(tempTUL$s2_SOSD20) ),]

tempTVK <- terra::extract(indxTVK, pt_coniferous, xy=T) %>% as.data.frame()
tempTVK <- tempTVK[-which(is.na(tempTVK$s1_SOSD17)),]
tempTVK <- tempTVK[which(is.na(tempTVK$s2_SOSD17) & 
                           is.na(tempTVK$s2_SOSD18) & 
                           is.na(tempTVK$s2_SOSD19) & 
                           is.na(tempTVK$s2_SOSD20) ),]

tempTVL <- terra::extract(indxTVL, pt_coniferous, xy=T) %>% as.data.frame()
tempTVL <- tempTVL[-which(is.na(tempTVL$s1_SOSD17)),]
tempTVL <- tempTVL[which(is.na(tempTVL$s2_SOSD17) & 
                           is.na(tempTVL$s2_SOSD18) & 
                           is.na(tempTVL$s2_SOSD19) & 
                           is.na(tempTVL$s2_SOSD20) ),]

# merge data frames and remove ID and s2 variables
df_temp <- rbind(tempTUK, tempTUL, tempTVK, tempTVL)
df_temp$ID <- NULL
df_temp$s2_SOSD17 <- NULL
df_temp$s2_SOSD18 <- NULL
df_temp$s2_SOSD19 <- NULL
df_temp$s2_SOSD20 <- NULL

df_coniferous <- df_temp
df_coniferous$class <- 'coniferous'

# some pixels might be duplicated because of overlapping tiles
unique(df_coniferous[,c('x','y')]) %>% nrow()
r2k <- unique(df_coniferous[,c('x','y')]) %>% rownames() # filter by rownames to reduce computation time
df_coniferous <- df_coniferous[r2k,]
unique(df_coniferous[,c('x','y')]) %>% nrow() # check

# change column names
colnames(df_coniferous)[1:48] <- substr(colnames(df_coniferous), 4, 9)[1:48]

# compute amplitude 
df_coniferous$AMPL17 <- df_coniferous$MAXV17 - df_coniferous$MINV17
df_coniferous$AMPL18 <- df_coniferous$MAXV18 - df_coniferous$MINV18
df_coniferous$AMPL19 <- df_coniferous$MAXV19 - df_coniferous$MINV19
df_coniferous$AMPL20 <- df_coniferous$MAXV20 - df_coniferous$MINV20

# order columns 
df_coniferous <- df_coniferous[,order(colnames(df_coniferous))]

# fix dates
df_coniferous$SOSD17 <- df_coniferous$SOSD17 - 17000
df_coniferous$EOSD17 <- df_coniferous$EOSD17 - 17000
df_coniferous$MAXD17 <- df_coniferous$MAXD17 - 17000
df_coniferous$SOSD18 <- df_coniferous$SOSD18 - 18000
df_coniferous$EOSD18 <- df_coniferous$EOSD18 - 18000
df_coniferous$MAXD18 <- df_coniferous$MAXD18 - 18000
df_coniferous$SOSD19 <- df_coniferous$SOSD19 - 19000
df_coniferous$EOSD19 <- df_coniferous$EOSD19 - 19000
df_coniferous$MAXD19 <- df_coniferous$MAXD19 - 19000
df_coniferous$SOSD20 <- df_coniferous$SOSD20 - 20000
df_coniferous$EOSD20 <- df_coniferous$EOSD20 - 20000
df_coniferous$MAXD20 <- df_coniferous$MAXD20 - 20000

# remove outliers
df_coniferous$EOSD17 %>% hist()
df_coniferous <- df_coniferous[-which(df_coniferous$EOSD17 > 400 | df_coniferous$EOSD18 > 400 | df_coniferous$EOSD19 > 400 | df_coniferous$EOSD20 > 400),]
df_coniferous$SOSD17 %>% hist()
df_coniferous <- df_coniferous[-which(df_coniferous$SOSD17 < 0 | df_coniferous$SOSD18 < 0 | df_coniferous$SOSD19 < 0 | df_coniferous$SOSD20 < 0),]

# retain complete cases
complete.cases(df_coniferous) %>% table()
df_coniferous <- df_coniferous[complete.cases(df_coniferous),]


# broadleaf ####

# extract values -> remove empty observations -> retain pixels with 1 season every year
tempTUK <- terra::extract(indxTUK, pt_broadleaf, xy=T) %>% as.data.frame()
tempTUK <- tempTUK[-which(is.na(tempTUK$s1_SOSD17)),]
tempTUK <- tempTUK[which(is.na(tempTUK$s2_SOSD17) & 
                           is.na(tempTUK$s2_SOSD18) & 
                           is.na(tempTUK$s2_SOSD19) & 
                           is.na(tempTUK$s2_SOSD20) ),]

tempTUL <- terra::extract(indxTUL, pt_broadleaf, xy=T) %>% as.data.frame()
tempTUL <- tempTUL[-which(is.na(tempTUL$s1_SOSD17)),]
tempTUL <- tempTUL[which(is.na(tempTUL$s2_SOSD17) & 
                           is.na(tempTUL$s2_SOSD18) & 
                           is.na(tempTUL$s2_SOSD19) & 
                           is.na(tempTUL$s2_SOSD20) ),]

tempTVK <- terra::extract(indxTVK, pt_broadleaf, xy=T) %>% as.data.frame()
tempTVK <- tempTVK[-which(is.na(tempTVK$s1_SOSD17)),]
tempTVK <- tempTVK[which(is.na(tempTVK$s2_SOSD17) & 
                           is.na(tempTVK$s2_SOSD18) & 
                           is.na(tempTVK$s2_SOSD19) & 
                           is.na(tempTVK$s2_SOSD20) ),]

tempTVL <- terra::extract(indxTVL, pt_broadleaf, xy=T) %>% as.data.frame()
tempTVL <- tempTVL[-which(is.na(tempTVL$s1_SOSD17)),]
tempTVL <- tempTVL[which(is.na(tempTVL$s2_SOSD17) & 
                           is.na(tempTVL$s2_SOSD18) & 
                           is.na(tempTVL$s2_SOSD19) & 
                           is.na(tempTVL$s2_SOSD20) ),]

# merge data frames and remove ID and s2 variables
df_temp <- rbind(tempTUK, tempTUL, tempTVK, tempTVL)
df_temp$ID <- NULL
df_temp$s2_SOSD17 <- NULL
df_temp$s2_SOSD18 <- NULL
df_temp$s2_SOSD19 <- NULL
df_temp$s2_SOSD20 <- NULL

df_broadleaf <- df_temp
df_broadleaf$class <- 'broadleaf'

# some pixels might be duplicated because of overlapping tiles
unique(df_broadleaf[,c('x','y')]) %>% nrow()
r2k <- unique(df_broadleaf[,c('x','y')]) %>% rownames() # filter by rownames to reduce computation time
df_broadleaf <- df_broadleaf[r2k,]
unique(df_broadleaf[,c('x','y')]) %>% nrow() # check

# change column names
colnames(df_broadleaf)[1:48] <- substr(colnames(df_broadleaf), 4, 9)[1:48]

# compute amplitude 
df_broadleaf$AMPL17 <- df_broadleaf$MAXV17 - df_broadleaf$MINV17
df_broadleaf$AMPL18 <- df_broadleaf$MAXV18 - df_broadleaf$MINV18
df_broadleaf$AMPL19 <- df_broadleaf$MAXV19 - df_broadleaf$MINV19
df_broadleaf$AMPL20 <- df_broadleaf$MAXV20 - df_broadleaf$MINV20

# order columns 
df_broadleaf <- df_broadleaf[,order(colnames(df_broadleaf))]

# fix dates
df_broadleaf$SOSD17 <- df_broadleaf$SOSD17 - 17000
df_broadleaf$EOSD17 <- df_broadleaf$EOSD17 - 17000
df_broadleaf$MAXD17 <- df_broadleaf$MAXD17 - 17000
df_broadleaf$SOSD18 <- df_broadleaf$SOSD18 - 18000
df_broadleaf$EOSD18 <- df_broadleaf$EOSD18 - 18000
df_broadleaf$MAXD18 <- df_broadleaf$MAXD18 - 18000
df_broadleaf$SOSD19 <- df_broadleaf$SOSD19 - 19000
df_broadleaf$EOSD19 <- df_broadleaf$EOSD19 - 19000
df_broadleaf$MAXD19 <- df_broadleaf$MAXD19 - 19000
df_broadleaf$SOSD20 <- df_broadleaf$SOSD20 - 20000
df_broadleaf$EOSD20 <- df_broadleaf$EOSD20 - 20000
df_broadleaf$MAXD20 <- df_broadleaf$MAXD20 - 20000

# remove outliers
df_broadleaf$EOSD17 %>% hist()
df_broadleaf <- df_broadleaf[-which(df_broadleaf$EOSD17 > 400 | df_broadleaf$EOSD18 > 400 | df_broadleaf$EOSD19 > 400 | df_broadleaf$EOSD20 > 400),]
df_broadleaf$SOSD17 %>% hist()
df_broadleaf <- df_broadleaf[-which(df_broadleaf$SOSD17 < 0 | df_broadleaf$SOSD18 < 0 | df_broadleaf$SOSD19 < 0 | df_broadleaf$SOSD20 < 0),]

# retain complete cases
complete.cases(df_broadleaf) %>% table()
df_broadleaf <- df_broadleaf[complete.cases(df_broadleaf),]


# rbind and write tiff ####
df_DLT_HRVPP <- rbind(df_coniferous[,colnames(df_coniferous)], df_broadleaf[,colnames(df_broadleaf)])
write.table(df_DLT_HRVPP, 'C:/Users/user/Desktop/CAPAS_ROI/df_DLT_HRVPP.txt')



