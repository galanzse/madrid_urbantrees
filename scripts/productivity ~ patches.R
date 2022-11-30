
source('scripts/study_area.R')
library(ggpubr)

# myvar
myvar <- c('TPRO17','TPRO18','TPRO19','TPRO20','MAXD17','MAXD18','MAXD19','MAXD20')

# import vegetation indexes
HRVPP_class2 <- read.csv("results/df_DLT_HRVPP_classified.txt", sep="") %>% dplyr::select(x,y,class2,all_of(myvar))
pt_HRVPP_class2 <- vect(HRVPP_class2, geom=c('x','y'), 'epsg:32630')
bf500_HRVPP_class2 <- buffer(pt_HRVPP_class2, 500)

ESM <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/ESM.tif')
ESM[ESM>0] <- 100; ESM[is.na(ESM)] <- 0
ESM <- terra::aggregate(ESM, fact=5, fun='mean')

HRVPP_class2$ESM500 <- ESM %>% terra::extract(bf500_HRVPP_class2) %>% group_by(ID) %>% summarise(Band_1=mean(Band_1)) %>%
  dplyr::select(Band_1) %>% deframe()

HRVPP_class2$urban <- cut(HRVPP_class2$ESM500, breaks=c(-1,0,33,66,100), labels=c('none','low','medium','high'))
HRVPP_class2 <- HRVPP_class2 %>% filter(urban %in% c('low','high'))

write.table(HRVPP_class2, 'results/PROMAX_class2.txt')

table(HRVPP_class2$class2, HRVPP_class2$urban)

# phenology
long_results <- HRVPP_class2 %>% gather(key='index', value='value', all_of(myvar))
long_results$index2 <- NA # add new variable to plot
long_results$index2[long_results$index %in% c('TPRO17','TPRO18','TPRO19','TPRO20')] <- 'TPRO'
long_results$index2[long_results$index %in% c('MAXD17','MAXD18','MAXD19','MAXD20')] <- 'MAXD'
long_results$index2 <- as.factor(long_results$index2)

long_results$year <- NA
long_results$year[long_results$index %in% c('TPRO17','MAXD17')] <- 2017
long_results$year[long_results$index %in% c('TPRO18','MAXD18')] <- 2018
long_results$year[long_results$index %in% c('TPRO19','MAXD19')] <- 2019
long_results$year[long_results$index %in% c('TPRO20','MAXD20')] <- 2020
long_results$year <- as.factor(long_results$year)

long_results <- na.omit(long_results)

g1 <- ggplot(aes(x=year, y=value, fill=urban), data=long_results[long_results$index2=='TPRO',]) +
  geom_boxplot(outlier.shape = NA) + ylab('TPRO') + xlab(NULL) +
  facet_wrap(~class2, nrow=1) +
  # coord_cartesian(ylim=c(-0,160)) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  theme_classic() + theme(legend.position="top", legend.title=element_blank()) +
  scale_fill_manual(values=c('white','grey80','grey50','grey20'))
g2 <- ggplot(aes(x=year, y=value, fill=urban), data=long_results[long_results$index2=='MAXD',]) +
  geom_boxplot(outlier.shape = NA) + ylab('MAXD') + xlab(NULL) +
  facet_wrap(~class2, nrow=1) +
  # coord_cartesian(ylim=c(125,365)) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  theme_classic() + theme(legend.position="none") +
  scale_fill_manual(values=c('white','grey80','grey50','grey20'))

ggarrange(g1,g2, nrow=2, heights=c(1.2,1))
