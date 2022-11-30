
source('scripts/study_area.R')
library(ggpubr)

# myvar
myvar <- c('EOSD17','EOSD18','EOSD19','EOSD20','LENG17','LENG18','LENG19','LENG20','SOSD17','SOSD18','SOSD19','SOSD20')

# import vegetation indexes
HRVPP_class <- read.csv("results/df_DLT_HRVPP_classified.txt", sep="") %>% dplyr::select(x,y,class2,all_of(myvar))
pt_HRVPP_class <- vect(HRVPP_class, geom=c('x','y'), 'epsg:32630')
bf250_HRVPP_class <- buffer(pt_HRVPP_class, 250)
bf500_HRVPP_class <- buffer(pt_HRVPP_class, 500)
bf1000_HRVPP_class <- buffer(pt_HRVPP_class, 1000)

# abiotic variables
HRVPP_class$elevation <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/DEM.tif') %>%
  terra::extract(pt_HRVPP_class) %>% dplyr::select(Band_1) %>% deframe()

ESM <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/ESM.tif')
ESM[ESM>0] <- 100; ESM[is.na(ESM)] <- 0
ESM <- terra::aggregate(ESM, fact=5, fun='mean')

HRVPP_class$ESM250 <- ESM %>% terra::extract(bf250_HRVPP_class) %>% group_by(ID) %>% summarise(Band_1=mean(Band_1)) %>%
  dplyr::select(Band_1) %>% deframe()
HRVPP_class$ESM500 <- ESM %>% terra::extract(bf500_HRVPP_class) %>% group_by(ID) %>% summarise(Band_1=mean(Band_1)) %>%
  dplyr::select(Band_1) %>% deframe()
HRVPP_class$ESM1000 <- ESM %>% terra::extract(bf1000_HRVPP_class) %>% group_by(ID) %>% summarise(Band_1=mean(Band_1)) %>%
  dplyr::select(Band_1) %>% deframe()

IMP <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/IMP.tif')
IMP <- terra::aggregate(IMP, fact=5, fun='mean')
HRVPP_class$IMP250 <- IMP %>% terra::extract(bf250_HRVPP_class) %>% group_by(ID) %>% summarise(AREA_KM2=mean(AREA_KM2)) %>%
  dplyr::select(AREA_KM2) %>% deframe()
HRVPP_class$IMP500 <- IMP %>% terra::extract(bf500_HRVPP_class) %>% group_by(ID) %>% summarise(AREA_KM2=mean(AREA_KM2)) %>%
  dplyr::select(AREA_KM2) %>% deframe()
HRVPP_class$IMP1000 <- IMP %>% terra::extract(bf1000_HRVPP_class) %>% group_by(ID) %>% summarise(AREA_KM2=mean(AREA_KM2)) %>%
  dplyr::select(AREA_KM2) %>% deframe()

MAR <- terra::rast('C:/Users/user/Desktop/wc2.1_30s_bio/wc2.1_30s_bio_12.tif') %>%
  terra::crop(terra::project(ESM, 'epsg:4326')) %>% terra::project('epsg:32630')
HRVPP_class$MAR <- MAR %>% terra::extract(bf500_HRVPP_class) %>% group_by(ID) %>% summarise(MAR=mean(wc2.1_30s_bio_12)) %>%
  dplyr::select(MAR) %>% deframe()

LST17 <- rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_17_night.tiff') %>% project('epsg:32630')
HRVPP_class$LST17_500 <- LST17 %>% terra::extract(bf500_HRVPP_class) %>% group_by(ID) %>% summarise(layer=mean(layer)) %>%
  dplyr::select(layer) %>% deframe()
LST18 <- rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_18_night.tiff') %>% project('epsg:32630')
HRVPP_class$LST18_500 <- LST18 %>% terra::extract(bf500_HRVPP_class) %>% group_by(ID) %>% summarise(layer=mean(layer)) %>%
  dplyr::select(layer) %>% deframe()
LST19 <- rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_19_night.tiff') %>% project('epsg:32630')
HRVPP_class$LST19_500 <- LST19 %>% terra::extract(bf500_HRVPP_class) %>% group_by(ID) %>% summarise(layer=mean(layer)) %>%
  dplyr::select(layer) %>% deframe()
LST20 <- rast('C:/Users/user/Desktop/CAPAS_ROI/MODIS_daynight_Madrid_20172020/LST_spring_20_night.tiff') %>% project('epsg:32630')
HRVPP_class$LST20_500 <- LST20 %>% terra::extract(bf500_HRVPP_class) %>% group_by(ID) %>% summarise(layer=mean(layer)) %>%
  dplyr::select(layer) %>% deframe()

# save
urb_HRVPP_class <- HRVPP_class
write.table(urb_HRVPP_class, 'results/urb_HRVPP_class.txt')
View(urb_HRVPP_class)


# urb_HRVPP_class
urb_HRVPP_class <- read.csv("results/urb_HRVPP_class.txt", sep="")
head(urb_HRVPP_class)
urb_HRVPP_class$class2 <- as.factor(urb_HRVPP_class$class2)
urb_HRVPP_class$urban <- cut(urb_HRVPP_class$ESM500, breaks=c(-1,0,33,66,100), labels=c('none','low','medium','high'))
# urb_HRVPP_class <- urb_HRVPP_class %>% filter(urban != 'none')

# spatial clustering
pt_urb_HRVPP_class <- vect(urb_HRVPP_class, geom=c('x','y'), 'epsg:32630')
# REF <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/ESM.tif') %>% aggregate(100, fun="min")
# urb_HRVPP_class$cell <- terra::extract(REF, pt_urb_HRVPP_class, cells=T) %>% dplyr::select(cell) %>% deframe()
# urb_HRVPP_class <- urb_HRVPP_class %>% group_by(class2,cell,urban) %>% sample_n(1)
table(urb_HRVPP_class$urban, urb_HRVPP_class$class2)

# maps
plot(terra::crop(MAR, STUDY_AREA))
deciduous <- urb_HRVPP_class[urb_HRVPP_class$class2=='deciduous',]
points(deciduous[,c('x','y')], col=deciduous$urban)
lines(MAD)
lines(MAD_PATCH, col='black')
coniferous <- urb_HRVPP_class[urb_HRVPP_class$class2=='coniferous',]
points(coniferous[,c('x','y')], col=deciduous$urban)
lines(MAD)
lines(MAD_PATCH, col='black')

# abiotic
urb_HRVPP_class[1:5000,c('ESM250','ESM500','ESM1000')] %>% pairs(lower.panel=NULL)
urb_HRVPP_class[1:5000,c('IMP250','IMP500','IMP1000')] %>% pairs(lower.panel=NULL)
urb_HRVPP_class[1:5000,c('ESM250','IMP500')] %>% pairs(lower.panel=NULL)
ggplot(aes(x=elevation, fill=urban), data=urb_HRVPP_class) + geom_density(alpha=.3) + facet_grid(~class2) # good
ggplot(aes(x=y, fill=urban), data=urb_HRVPP_class) + geom_density(alpha=.3) + facet_grid(~class2) # good
ggplot(aes(x=MAR, fill=urban), data=urb_HRVPP_class) + geom_density(alpha=.3) + facet_grid(~class2) # good

long_results <- urb_HRVPP_class[,c('class2','ESM500','IMP500','LST17_500','LST18_500','LST19_500','LST20_500','urban')] %>%
  gather(key='index', value='LST', all_of(c('LST17_500','LST18_500','LST19_500','LST20_500')))
long_results$year <- NA
long_results$year[long_results$index == 'LST17_500'] <- 2017
long_results$year[long_results$index == 'LST18_500'] <- 2018
long_results$year[long_results$index == 'LST19_500'] <- 2019
long_results$year[long_results$index == 'LST20_500'] <- 2020
long_results$year <- as.factor(long_results$year)
ggplot(aes(x=year, y=LST, fill=urban), data=long_results) +
  geom_boxplot(outlier.shape = NA) + ylab('LST (ºC)') + xlab(NULL) +
  facet_wrap(~class2, nrow=1) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  theme_classic() + theme(legend.position="top", legend.title=element_blank()) +
  scale_fill_manual(values=c('white','grey80','grey50','grey20'))

# unique(ggplot_build(a)$data[[1]]["fill"])

# phenology
long_results <- urb_HRVPP_class %>% gather(key='index', value='value', all_of(myvar))
long_results$index2 <- NA # add new variable to plot
long_results$index2[long_results$index %in% c('SOSD17','SOSD18','SOSD19','SOSD20')] <- 'SOS'
long_results$index2[long_results$index %in% c('LENG17','LENG18','LENG19','LENG20')] <- 'LES'
long_results$index2[long_results$index %in% c('EOSD17','EOSD18','EOSD19','EOSD20')] <- 'EOS'
long_results$index2 <- as.factor(long_results$index2)
long_results$index2 <- factor(long_results$index2, levels=c("SOS","LES","EOS"))

long_results$year <- NA
long_results$year[long_results$index %in% c('SOSD17','LENG17','EOSD17')] <- 2017
long_results$year[long_results$index %in% c('SOSD18','LENG18','EOSD18')] <- 2018
long_results$year[long_results$index %in% c('SOSD19','LENG19','EOSD19')] <- 2019
long_results$year[long_results$index %in% c('SOSD20','LENG20','EOSD20')] <- 2020
long_results$year <- as.factor(long_results$year)

g1 <- ggplot(aes(x=year, y=value, fill=urban), data=long_results[long_results$index2=='SOS',]) +
  geom_boxplot(outlier.shape = NA) + ylab('SOS') + xlab(NULL) +
  facet_wrap(~class2, nrow=1) +
  coord_cartesian(ylim=c(-0,160)) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  theme_classic() + theme(legend.position="top", legend.title=element_blank()) +
  scale_fill_manual(values=c('white','grey80','grey50','grey20'))
g2 <- ggplot(aes(x=year, y=value, fill=urban), data=long_results[long_results$index2=='LES',]) +
  geom_boxplot(outlier.shape = NA) + ylab('LES') + xlab(NULL) +
  facet_wrap(~class2, nrow=1) +
  coord_cartesian(ylim=c(125,365)) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  theme_classic() + theme(legend.position="none") +
  scale_fill_manual(values=c('white','grey80','grey50','grey20'))
g3 <- ggplot(aes(x=year, y=value, fill=urban), data=long_results[long_results$index2=='EOS',]) +
  geom_boxplot(outlier.shape = NA) + ylab('EOS') + xlab(NULL) +
  facet_wrap(~class2, nrow=1) +
  coord_cartesian(ylim=c(250,365)) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  theme_classic() + theme(legend.position="none") +
  scale_fill_manual(values=c('white','grey80','grey50','grey20'))

ggarrange(g1,g2,g3, nrow=3, heights=c(1.2,1,1))

# tests
TukeyHSD(aov(SOSD17 ~ urban, data=coniferous), conf.level=0.95)
TukeyHSD(aov(SOSD18 ~ urban, data=coniferous), conf.level=0.95)
TukeyHSD(aov(SOSD19 ~ urban, data=coniferous), conf.level=0.95)
TukeyHSD(aov(SOSD20 ~ urban, data=coniferous), conf.level=0.95)

TukeyHSD(aov(LENG17 ~ urban, data=coniferous), conf.level=0.95)
TukeyHSD(aov(LENG18 ~ urban, data=coniferous), conf.level=0.95)
TukeyHSD(aov(LENG19 ~ urban, data=coniferous), conf.level=0.95)
TukeyHSD(aov(LENG20 ~ urban, data=coniferous), conf.level=0.95)

TukeyHSD(aov(EOSD17 ~ urban, data=coniferous), conf.level=0.95)
TukeyHSD(aov(EOSD18 ~ urban, data=coniferous), conf.level=0.95)
TukeyHSD(aov(EOSD19 ~ urban, data=coniferous), conf.level=0.95)
TukeyHSD(aov(EOSD20 ~ urban, data=coniferous), conf.level=0.95)

TukeyHSD(aov(SOSD17 ~ urban, data=deciduous), conf.level=0.95)
TukeyHSD(aov(SOSD18 ~ urban, data=deciduous), conf.level=0.95)
TukeyHSD(aov(SOSD19 ~ urban, data=deciduous), conf.level=0.95)
TukeyHSD(aov(SOSD20 ~ urban, data=deciduous), conf.level=0.95)

TukeyHSD(aov(LENG17 ~ urban, data=deciduous), conf.level=0.95)
TukeyHSD(aov(LENG18 ~ urban, data=deciduous), conf.level=0.95)
TukeyHSD(aov(LENG19 ~ urban, data=deciduous), conf.level=0.95)
TukeyHSD(aov(LENG20 ~ urban, data=deciduous), conf.level=0.95)

TukeyHSD(aov(EOSD17 ~ urban, data=deciduous), conf.level=0.95)
TukeyHSD(aov(EOSD18 ~ urban, data=deciduous), conf.level=0.95)
TukeyHSD(aov(EOSD19 ~ urban, data=deciduous), conf.level=0.95)
TukeyHSD(aov(EOSD20 ~ urban, data=deciduous), conf.level=0.95)

TukeyHSD(aov(LST17_500 ~ urban, data=deciduous), conf.level=0.95)
TukeyHSD(aov(LST18_500 ~ urban, data=deciduous), conf.level=0.95)
TukeyHSD(aov(LST19_500 ~ urban, data=deciduous), conf.level=0.95)
TukeyHSD(aov(LST20_500 ~ urban, data=deciduous), conf.level=0.95)

TukeyHSD(aov(LST17_500 ~ urban, data=coniferous), conf.level=0.95)
TukeyHSD(aov(LST18_500 ~ urban, data=coniferous), conf.level=0.95)
TukeyHSD(aov(LST19_500 ~ urban, data=coniferous), conf.level=0.95)
TukeyHSD(aov(LST20_500 ~ urban, data=coniferous), conf.level=0.95)
