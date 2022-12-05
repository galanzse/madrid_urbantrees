
library(tidyverse)
library(caret)
library(caTools)

# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

# means dataset
df_GE_med <- read.csv("results/df_GE_med.txt", sep="")
df_GE_med$class <- as.factor(df_GE_med$class)
str(df_GE_med)

myvar <- colnames(df_GE_med)[4:16] # traits

# remove SOSV and EOSV because of their high correlation with MINV
rf_var <- setdiff(myvar, c('SOSV','EOSV'))
data_rf <- df_GE_med %>% dplyr::select(class, all_of(rf_var))

# 10 percent of data for external validation
ind_in <- sample(1:nrow(data_rf), nrow(data_rf)/10)
data_ex <- data_rf[ind_in,]; table(data_ex$class)
data_rf <- data_rf[-ind_in,]

# 5-fold cross validation 
control <- trainControl(method="repeatedcv", number=5, repeats=5, search="grid")
rf_random <- train(class~., data=data_rf, method="rf", metric="Accuracy", trControl=control)
save(rf_random, file="results/rf_random.RData")
rf_random
plot(rf_random)
varImp(rf_random, scale=F)
plot(varImp(rf_random, scale=F))

# confusion matrix
predicted_class <- predict(rf_random, newdata=data_ex[,rf_var], type='raw')
confusionMatrix(data=predicted_class, reference=data_ex$class)


# classify broadleaf category from DLT

# import dataset
df_DLT_HRVPP <- read.csv("results/df_DLT_HRVPP.txt", sep="")

# traits means for classification
df_DLT_HRVPP_means <- df_DLT_HRVPP[,c('x','y','class')]
df_DLT_HRVPP_means$LENG <- df_DLT_HRVPP[,c("LENG17","LENG18","LENG19","LENG20")] %>% rowMeans()
df_DLT_HRVPP_means$LSLO <- df_DLT_HRVPP[,c("LSLO17","LSLO18","LSLO19","LSLO20")] %>% rowMeans()
df_DLT_HRVPP_means$MAXV <- df_DLT_HRVPP[,c("MAXV17","MAXV18","MAXV19","MAXV20")] %>% rowMeans()
df_DLT_HRVPP_means$MINV <- df_DLT_HRVPP[,c("MINV17","MINV18","MINV19","MINV20")] %>% rowMeans()
df_DLT_HRVPP_means$RSLO <- df_DLT_HRVPP[,c("RSLO17","RSLO18","RSLO19","RSLO20")] %>% rowMeans()
df_DLT_HRVPP_means$SOSD <- df_DLT_HRVPP[,c("SOSD17","SOSD18","SOSD19","SOSD20")] %>% rowMeans()
df_DLT_HRVPP_means$SPRO <- df_DLT_HRVPP[,c("SPRO17","SPRO18","SPRO19","SPRO20")] %>% rowMeans()
df_DLT_HRVPP_means$TPRO <- df_DLT_HRVPP[,c("TPRO17","TPRO18","TPRO19","TPRO20")] %>% rowMeans()
df_DLT_HRVPP_means$AMPL <- df_DLT_HRVPP[,c("AMPL17","AMPL18","AMPL19","AMPL20")] %>% rowMeans()
df_DLT_HRVPP_means$EOSD <- df_DLT_HRVPP[,c("EOSD17","EOSD18","EOSD19","EOSD20")] %>% rowMeans()
df_DLT_HRVPP_means$MAXD <- df_DLT_HRVPP[,c("MAXD17","MAXD18","MAXD19","MAXD20")] %>% rowMeans()
df_DLT_HRVPP_means$EOSV <- df_DLT_HRVPP[,c("EOSV17","EOSV18","EOSV19","EOSV20")] %>% rowMeans()
df_DLT_HRVPP_means$SOSV <- df_DLT_HRVPP[,c("SOSV17","SOSV18","SOSV19","SOSV20")] %>% rowMeans()

# assign classes using random forest model
rownames(df_DLT_HRVPP_means) <- 1:nrow(df_DLT_HRVPP_means)
pred1 <- predict(rf_random, newdata=df_DLT_HRVPP_means[,rf_var], type='prob') %>% as.data.frame()
pred1$class2 <- colnames(pred1)[max.col(pred1)]
pred1$max <- as.vector(apply(pred1[c('coniferous','sclerophyllous','deciduous','golf','managed')], 1, max))

# merge original data
df_DLT_HRVPP_means <- cbind(df_DLT_HRVPP_means, pred1[,c('class2','max')])

# plot
temp <- df_DLT_HRVPP_means %>% dplyr::select(class2,SOSD,LSLO,MINV)

# TukeyHSD(aov(SOSD ~ class2, data=temp), conf.level=0.95)
# TukeyHSD(aov(LSLO ~ class2, data=temp), conf.level=0.95)
# TukeyHSD(aov(MINV ~ class2, data=temp), conf.level=0.95)

temp <- temp %>% gather(2:4, key='trait', value='value')
temp$trait <- as.factor(temp$trait)
levels(temp$trait) <- c("LSLO","MINV","SOS")
temp$trait <- factor(temp$trait, levels=c('SOS','LSLO','MINV'))
temp$class2 <- as.factor(temp$class2)
temp$class2 <- factor(temp$class2, levels=c('coniferous','deciduous','sclerophyllous','managed','golf'))
ggplot(aes(x=class2, y=value), data=temp) + facet_wrap(~trait, scales="free_y") +
  geom_boxplot() +
  xlab('') + ylab('') + theme_classic() +
  scale_x_discrete(guide=guide_axis(angle = -45))

# compare classifications
table(df_DLT_HRVPP_means$class, df_DLT_HRVPP_means$class2)

# keep coniferous and deciduous broadleaf
coniferous <- df_DLT_HRVPP_means %>% filter(class=='coniferous' & class2=='coniferous')
deciduous <- df_DLT_HRVPP_means %>% filter(class=='broadleaf' & class2=='deciduous')
df_DLT_HRVPP_means <- rbind(deciduous,coniferous)

# threshold
hist(pred1$max) # umbral aleatorio
table(pred1$class2[pred1$max > 0.80])
df_DLT_HRVPP_means <- df_DLT_HRVPP_means %>% subset(max > 0.80)

# final data for analyses
table(df_DLT_HRVPP_means$class2)

# merge classification with yearly indexes
df_DLT_HRVPP_classified <- merge(df_DLT_HRVPP, df_DLT_HRVPP_means[,c('x','y','class','class2')], by=c('x','y','class'))

# save
write.table(df_DLT_HRVPP_classified, 'results/df_DLT_HRVPP_classified.txt')

# visual inspection
# out_ever <- df_DLT_HRVPP_classified %>% group_by(class2) %>% sample_n(100) %>% dplyr::select(x,y,class2) %>% as.data.frame()
# out_ever <- vect(out_ever, geom=c('x','y'), 'epsg:32630') %>% terra::project('epsg:4326') %>% terra::as.data.frame(geom='XY')
# write.table(out_ever, 'results/out_ever.txt', row.names=F)

# plot
long_df_DLT_means <- df_DLT_HRVPP_means %>% gather(4:16, key='trait', value='value')
ggplot(aes(x=class2, y=value), data=long_df_DLT_means) + geom_boxplot() + facet_wrap(~trait, scales = "free_y")

