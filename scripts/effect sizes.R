
library(tidyverse)
# library(plyr) # careful!! this package alters the behaviour of group_by

# import data
data <- read.csv("results/urb_HRVPP_class.txt", sep="")
data$urban <- cut(data$ESM500, breaks=c(0,33,66,100), labels=c('low','medium','high'))
data <- na.omit(data)

# round elevation to facilitate control
# data$elevation <- data$elevation %>% round(0)

# spatial clustering
ESM <- terra::rast('C:/Users/user/Desktop/CAPAS_ROI/ESM.tif') %>% aggregate(100, fun="min")
pt_data <- vect(data, geom=c('x','y'), 'epsg:32630')
data$cell <- terra::extract(ESM, pt_data, cells=T) %>% dplyr::select(cell) %>% deframe()

# nrun
nrun <- 100

# effect sizes for the comparison High vs Low urbanization
ES_pheno <- expand.grid(myvar, c('deciduous','coniferous'))
colnames(ES_pheno) <- c('index','class')
ES_pheno <- cbind(ES_pheno, matrix(ncol=nrun, nrow=nrow(ES_pheno)))

for (i in 1:nrow(ES_pheno)) {
  low <- data %>% filter(class2==ES_pheno[i,'class'] & urban=='low') %>% dplyr::select(elevation, cell, ES_pheno[i,'index'])
  high <- data %>% filter(class2==ES_pheno[i,'class'] & urban=='high') %>% dplyr::select(elevation, cell, ES_pheno[i,'index'])

  for (r in 1:nrun) {
    # randomly select 1 obs x cell and 100 observations
    r_low <- low %>% group_by(cell) %>% sample_n(1); r_low <- r_low[sample(1:nrow(r_low),100),] %>% as.data.frame()
    # select observations within observed range
    r_high <- high %>% filter(between(elevation, min(r_low$elevation), max(r_low$elevation))) %>% sample_n(100) %>% as.data.frame()
    # boxplot(r_low$elevation, r_high$elevation)
    ES_pheno[i,as.character(r)] <- mean(r_high[,3]) - mean(r_low[,3])
  }
  
  print(i)
}

# save
write.table(ES_pheno, 'results/ES_pheno.txt')

# plot
ES_long <- ES_pheno %>% gather(3:ncol(ES_pheno), key='run', value='value')
ES_long$run <- NULL
ES_long$index2 <- NA
ES_long$index2[ES_long$index %in% c("SOSD17","SOSD18","SOSD19","SOSD20")] <- 'SOS'
ES_long$index2[ES_long$index %in% c("LENG17","LENG18","LENG19","LENG20")] <- 'LES'
ES_long$index2[ES_long$index %in% c("EOSD17","EOSD18","EOSD19","EOSD20")] <- 'EOS'
ES_long$index2 <- as.factor(ES_long$index2)
ES_long$index2 <- factor(ES_long$index2, levels=c("SOS","LES","EOS"))

ES_long$year <- NA
ES_long$year[ES_long$index %in% c('SOSD17','LENG17','EOSD17')] <- 2017
ES_long$year[ES_long$index %in% c('SOSD18','LENG18','EOSD18')] <- 2018
ES_long$year[ES_long$index %in% c('SOSD19','LENG19','EOSD19')] <- 2019
ES_long$year[ES_long$index %in% c('SOSD20','LENG20','EOSD20')] <- 2020
ES_long$year <- as.factor(ES_long$year)

ES_long$class <- as.factor(ES_long$class)

ggplot(aes(x=year, y=value), data=ES_long) +
  geom_boxplot(outlier.shape = NA) + ylab(NULL) + xlab(NULL) +
  facet_wrap(~class+index2, nrow=2) +
  geom_hline(yintercept=0) +
  theme(axis.text.x=element_text(angle = -45, hjust = 0)) + ylab('days') +
  theme_classic() + theme(legend.position="none", legend.title=element_blank()) +
  scale_fill_manual(values=c('white','grey80','grey50','grey20'))

# sign
for (r in 1:nrow(ES_pheno)) {
  temp <- ES_pheno[r,3:102] %>% t() %>% as.vector()
  paste('The p-value of', ES_pheno[r,1], 'for', ES_pheno[r,2], 'is', round(wilcox.test(temp)$p.value,3)) %>% print()
}
