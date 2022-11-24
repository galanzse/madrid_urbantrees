
library(sf)
source('scripts/study_area.R')

# project region of interest
STUDY_AREA30 <- STUDY_AREA %>% terra::project('epsg:3035')

# find my cells of interest
LAEA_100grid <- terra::vect('E:/DLT_2018_010m_es_03035_v020/es_LAEA/ES_100K.shp') %>% terra::crop(STUDY_AREA30)
plot(LAEA_100grid)
CellCode <- unique(LAEA_100grid$CellCode)

# import DLT and mask by TCD and DEM
DLT_list <- list()
for (i in 1:length(CellCode)) {
  
  # import TCD
  myTCD <- paste('E:/TCD_2018_010m_es_03035_v020/DATA/TCD_2018_010m_',
                  substr(CellCode[i],6,13), '_03035_v020.tif', sep='') %>% rast() %>%
                  terra::crop(STUDY_AREA30, mask=T) %>% as.numeric() # terra::is.factor(myTCD)
  # set 75% cover threshold
  myTCD[myTCD<75] <- NA; myTCD[myTCD>=75] <- 1
  # points file
  myTCD <- terra::as.data.frame(myTCD, xy=T)
  myTCD <- myTCD %>% vect(geom=c('x','y'), 'epsg:3035')
  
  # import DLT and extract
  myDLT <- paste('E:/DLT_2018_010m_es_03035_v020/DATA/DLT_2018_010m_',
                  substr(CellCode[i],6,13), '_03035_v020.tif', sep='') %>% rast()
  myDLT <- as.numeric(myDLT); myDLT[myDLT==0] <- NA # retrieve classes
  
  # extract, project and add to list
  myDLT <- terra::extract(myDLT, myTCD, xy=T); colnames(myDLT)[colnames(myDLT)=='Value'] <- 'class'
  myDLT <- myDLT %>% vect(geom=c('x','y'), 'epsg:3035') %>% terra::project('epsg:32630') %>% terra::as.data.frame(geom='xy')

  # save and status
  DLT_list[[i]] <- myDLT[,c('class','x','y')]
  print(i/length(CellCode))
}

# save list
save(DLT_list, file='results/DLT_list.Rdata')

# merge
sapply(DLT_list, nrow) # nrow
df_DLT <- do.call(rbind, DLT_list)

# class as factor
hist(df_DLT$class)
df_DLT <- df_DLT %>% filter(class %in% c(1,2))
df_DLT$class <- as.factor(df_DLT$class)
levels(df_DLT$class) <- c('broadleaf','coniferous')

# write table
table(df_DLT$class)
write.table(df_DLT, file='results/df_DLT.txt')


# HR-VPP data
pt_DLT <- vect(df_DLT, geom=c('x','y'), 'epsg:32630') # points

# mask DLT by DEM, and new point file
DEM <- rast('C:/Users/user/Desktop/CAPAS_ROI/DEM.tif')
df_DLT$elevation <- DEM %>% terra::extract(pt_DLT) %>% dplyr::select(Band_1) %>% deframe()
df_DLT <- df_DLT %>% filter(between(elevation, DEMmin, DEMmax))
pt_DLT <- vect(df_DLT, geom=c('x','y'), 'epsg:32630') # new point file

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

# extract values -> remove empty observations -> retain pixels with 1 season every year
tempTUK <- terra::extract(indxTUK, pt_DLT, xy=T) %>% as.data.frame()
tempTUK$class <- pt_DLT$class
tempTUK <- tempTUK[-which(is.na(tempTUK$s1_SOSD17)),]
tempTUK <- tempTUK[which(is.na(tempTUK$s2_SOSD17) & is.na(tempTUK$s2_SOSD18) & is.na(tempTUK$s2_SOSD19) & is.na(tempTUK$s2_SOSD20) ),]

tempTUL <- terra::extract(indxTUL, pt_DLT, xy=T) %>% as.data.frame()
tempTUL$class <- pt_DLT$class
tempTUL <- tempTUL[-which(is.na(tempTUL$s1_SOSD17)),]
tempTUL <- tempTUL[which(is.na(tempTUL$s2_SOSD17) & is.na(tempTUL$s2_SOSD18) & is.na(tempTUL$s2_SOSD19) & is.na(tempTUL$s2_SOSD20) ),]

tempTVK <- terra::extract(indxTVK, pt_DLT, xy=T) %>% as.data.frame()
tempTVK$class <- pt_DLT$class
tempTVK <- tempTVK[-which(is.na(tempTVK$s1_SOSD17)),]
tempTVK <- tempTVK[which(is.na(tempTVK$s2_SOSD17) & is.na(tempTVK$s2_SOSD18) & is.na(tempTVK$s2_SOSD19) & is.na(tempTVK$s2_SOSD20) ),]

tempTVL <- terra::extract(indxTVL, pt_DLT, xy=T) %>% as.data.frame()
tempTVL$class <- pt_DLT$class
tempTVL <- tempTVL[-which(is.na(tempTVL$s1_SOSD17)),]
tempTVL <- tempTVL[which(is.na(tempTVL$s2_SOSD17) & is.na(tempTVL$s2_SOSD18) & is.na(tempTVL$s2_SOSD19) & is.na(tempTVL$s2_SOSD20) ),]

# merge data frames and remove ID and s2 variables
df_DLT_HRVPP <- rbind(tempTUK, tempTUL, tempTVK, tempTVL)
df_DLT_HRVPP$ID <- NULL
df_DLT_HRVPP$s2_SOSD17 <- NULL
df_DLT_HRVPP$s2_SOSD18 <- NULL
df_DLT_HRVPP$s2_SOSD19 <- NULL
df_DLT_HRVPP$s2_SOSD20 <- NULL

# some pixels might be duplicated because of overlapping tiles
unique(df_DLT_HRVPP[,c('x','y')]) %>% nrow()
r2k <- unique(df_DLT_HRVPP[,c('x','y')]) %>% rownames() # filter by rownames to reduce computation time
df_DLT_HRVPP <- df_DLT_HRVPP[r2k,]
unique(df_DLT_HRVPP[,c('x','y')]) %>% nrow() # check

# change column names
colnames(df_DLT_HRVPP)[1:48] <- substr(colnames(df_DLT_HRVPP), 4, 9)[1:48]

# compute amplitude 
df_DLT_HRVPP$AMPL17 <- df_DLT_HRVPP$MAXV17 - df_DLT_HRVPP$MINV17
df_DLT_HRVPP$AMPL18 <- df_DLT_HRVPP$MAXV18 - df_DLT_HRVPP$MINV18
df_DLT_HRVPP$AMPL19 <- df_DLT_HRVPP$MAXV19 - df_DLT_HRVPP$MINV19
df_DLT_HRVPP$AMPL20 <- df_DLT_HRVPP$MAXV20 - df_DLT_HRVPP$MINV20

# fix dates
df_DLT_HRVPP$SOSD17 <- df_DLT_HRVPP$SOSD17 - 17000
df_DLT_HRVPP$EOSD17 <- df_DLT_HRVPP$EOSD17 - 17000
df_DLT_HRVPP$MAXD17 <- df_DLT_HRVPP$MAXD17 - 17000
df_DLT_HRVPP$SOSD18 <- df_DLT_HRVPP$SOSD18 - 18000
df_DLT_HRVPP$EOSD18 <- df_DLT_HRVPP$EOSD18 - 18000
df_DLT_HRVPP$MAXD18 <- df_DLT_HRVPP$MAXD18 - 18000
df_DLT_HRVPP$SOSD19 <- df_DLT_HRVPP$SOSD19 - 19000
df_DLT_HRVPP$EOSD19 <- df_DLT_HRVPP$EOSD19 - 19000
df_DLT_HRVPP$MAXD19 <- df_DLT_HRVPP$MAXD19 - 19000
df_DLT_HRVPP$SOSD20 <- df_DLT_HRVPP$SOSD20 - 20000
df_DLT_HRVPP$EOSD20 <- df_DLT_HRVPP$EOSD20 - 20000
df_DLT_HRVPP$MAXD20 <- df_DLT_HRVPP$MAXD20 - 20000

# remove outliers
df_DLT_HRVPP$EOSD17 %>% hist()
df_DLT_HRVPP <- df_DLT_HRVPP[-which(df_DLT_HRVPP$EOSD17 > 400 | df_DLT_HRVPP$EOSD18 > 400 | df_DLT_HRVPP$EOSD19 > 400 | df_DLT_HRVPP$EOSD20 > 400),]
df_DLT_HRVPP$SOSD17 %>% hist()
df_DLT_HRVPP <- df_DLT_HRVPP[-which(df_DLT_HRVPP$SOSD17 < 0 | df_DLT_HRVPP$SOSD18 < 0 | df_DLT_HRVPP$SOSD19 < 0 | df_DLT_HRVPP$SOSD20 < 0),]

# retain complete cases
complete.cases(df_DLT_HRVPP) %>% table()
df_DLT_HRVPP <- df_DLT_HRVPP[complete.cases(df_DLT_HRVPP),]

# functional differences between vegetation types
par(mfrow=c(1,4), mar=c(4,4,4,4))
boxplot(df_DLT_HRVPP$MINV17 ~ df_DLT_HRVPP$class)
boxplot(df_DLT_HRVPP$SOSD17 ~ df_DLT_HRVPP$class)
boxplot(df_DLT_HRVPP$EOSD17 ~ df_DLT_HRVPP$class)
boxplot(df_DLT_HRVPP$LSLO17 ~ df_DLT_HRVPP$class)

# write tiff
write.table(df_DLT_HRVPP, 'results/df_DLT_HRVPP.txt')
