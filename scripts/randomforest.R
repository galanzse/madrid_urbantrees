
library(randomForest)
library(tidyverse)
library(caret)
library(caTools)

# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

# means dataset
df_GE_med <- read.csv("results/df_GE_med.txt", sep="")
df_GE_med$class <- as.factor(df_GE_med$class)
str(df_GE_med)

myvar <- colnames(df_GE_med)[4:16] # traits

# remove SOSV y EOSV because of their correlation with MINV
# remove EOSD y MAXD as they dont look important in the boxplots
rf_var <- setdiff(myvar, c('SOSV','EOSV','EOSD','MAXD'))
# remove category managed
data_rf <- df_GE_med %>% dplyr::select(class, all_of(rf_var)) %>% subset(class != 'managed')
# factorize classes
data_rf$class <- data_rf$class %>% as.character() %>% as.factor()

# get best ntry
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
# rf_random <- train(class~., data=data_rf, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
# print(rf_random)
# plot(rf_random)

d_sample = sample.split(data_rf$class, SplitRatio = .75)
d_train = subset(data_rf, d_sample == TRUE)
d_test  = subset(data_rf, d_sample == FALSE)

par(mfrow=c(1,1))
rf1 <- randomForest(class~., data=d_train, ntree=1000, mtry=2, replace=T, importance=T)
rf1
plot(rf1)
rf1$importance
varImpPlot(rf1, main=NULL)

pred = predict(rf1, newdata=d_test[-1]) # cross-validation
(cm = table(d_test[,1], pred))

# accuracy
(sum(diag(cm))/sum(cm))

# precision
diag(cm)/colSums(cm)

# recall
diag(cm)/rowSums(cm)

# F1
2*(diag(cm)/colSums(cm) * diag(cm)/rowSums(cm))/(diag(cm)/colSums(cm) + diag(cm)/rowSums(cm))


# classify broadleaf category from DLT

# import dataset
df_DLT_HRVPP <- read.csv("C:/Users/user/Desktop/CAPAS_ROI/df_DLT_HRVPP.txt", sep="")

# filter broadleaf
df_broadleaf <- df_DLT_HRVPP %>% filter(class=='broadleaf')
df_broadleaf$class <- NULL

# traits means for classification
df_broadleaf_means <- df_broadleaf[,c('x','y')]
df_broadleaf_means$LENG <- df_broadleaf[,c("LENG17","LENG18","LENG19","LENG20")] %>% rowMeans()
df_broadleaf_means$LSLO <- df_broadleaf[,c("LSLO17","LSLO18","LSLO19","LSLO20")] %>% rowMeans()
df_broadleaf_means$MAXV <- df_broadleaf[,c("MAXV17","MAXV18","MAXV19","MAXV20")] %>% rowMeans()
df_broadleaf_means$MINV <- df_broadleaf[,c("MINV17","MINV18","MINV19","MINV20")] %>% rowMeans()
df_broadleaf_means$RSLO <- df_broadleaf[,c("RSLO17","RSLO18","RSLO19","RSLO20")] %>% rowMeans()
df_broadleaf_means$SOSD <- df_broadleaf[,c("SOSD17","SOSD18","SOSD19","SOSD20")] %>% rowMeans()
df_broadleaf_means$SPRO <- df_broadleaf[,c("SPRO17","SPRO18","SPRO19","SPRO20")] %>% rowMeans()
df_broadleaf_means$TPRO <- df_broadleaf[,c("TPRO17","TPRO18","TPRO19","TPRO20")] %>% rowMeans()
df_broadleaf_means$AMPL <- df_broadleaf[,c("AMPL17","AMPL18","AMPL19","AMPL20")] %>% rowMeans()

# assign classes using random forest model
rownames(df_broadleaf_means) <- 1:nrow(df_broadleaf_means)
pred1 <- predict(rf1, newdata=df_broadleaf_means[,rf_var], type='prob') %>% as.data.frame()
pred1$class <- predict(rf1, newdata=df_broadleaf_means[,rf_var], type='class') %>% as.vector()
# cbind
pred1 <- cbind(pred1, as.vector(apply(pred1[c('deciduous','evergreen','golf')], 1, max)))
colnames(pred1) <- c('deciduous','evergreen','golf','class','max')
pred1 <- pred1[,c('class','max')]

hist(pred1$max) # umbral aleatorio
table(pred1$max > 0.95)

# merge original data
df_broadleaf_means <- cbind(df_broadleaf_means, pred1)
df_broadleaf_means <- df_broadleaf_means %>% subset(max > 0.95)

# elimino golf
df_broadleaf_means <- df_broadleaf_means %>% subset(class != 'golf')
table(df_broadleaf_means$class)

# merge classification with yearly indexes
df_broadleaf_classified <- merge(df_broadleaf, df_broadleaf_means[,c('x','y','class')], by=c('x','y'))

# rbind with coniferous
df_coniferous <- df_DLT_HRVPP %>% filter(class=='coniferous')
df_broadleaf_classified
df_DLT_HRVPP_classified <- rbind(df_coniferous, df_broadleaf_classified[,colnames(df_coniferous)])

# visual inspection
out_ever <- df_DLT_HRVPP_classified %>% group_by(class) %>% sample_n(500) %>% dplyr::select(x,y,class) %>% as.data.frame()
out_ever <- vect(out_ever, geom=c('x','y'), 'epsg:32630') %>% project('epsg:4326') %>% terra::as.data.frame(geom='XY')
write.table(out_ever, 'results/out_ever.txt', row.names=F)

# save
write.table(df_DLT_HRVPP_classified, 'results/df_DLT_HRVPP_classified.txt')


# plot
df_DLT_means_classified <- df_DLT_HRVPP_classified[,c('x','y','class')]
df_DLT_means_classified$EOSD <- df_DLT_HRVPP_classified[,c("EOSD17","EOSD18","EOSD19","EOSD20")] %>% rowMeans()
df_DLT_means_classified$EOSV <- df_DLT_HRVPP_classified[,c("EOSV17","EOSV18","EOSV19","EOSV20")] %>% rowMeans()
df_DLT_means_classified$LENG <- df_DLT_HRVPP_classified[,c("LENG17","LENG18","LENG19","LENG20")] %>% rowMeans()
df_DLT_means_classified$LSLO <- df_DLT_HRVPP_classified[,c("LSLO17","LSLO18","LSLO19","LSLO20")] %>% rowMeans()
df_DLT_means_classified$MAXD <- df_DLT_HRVPP_classified[,c("MAXD17","MAXD18","MAXD19","MAXD20")] %>% rowMeans()
df_DLT_means_classified$MAXV <- df_DLT_HRVPP_classified[,c("MAXV17","MAXV18","MAXV19","MAXV20")] %>% rowMeans()
df_DLT_means_classified$MINV <- df_DLT_HRVPP_classified[,c("MINV17","MINV18","MINV19","MINV20")] %>% rowMeans()
df_DLT_means_classified$RSLO <- df_DLT_HRVPP_classified[,c("RSLO17","RSLO18","RSLO19","RSLO20")] %>% rowMeans()
df_DLT_means_classified$SOSD <- df_DLT_HRVPP_classified[,c("SOSD17","SOSD18","SOSD19","SOSD20")] %>% rowMeans()
df_DLT_means_classified$SOSV <- df_DLT_HRVPP_classified[,c("SOSV17","SOSV18","SOSV19","SOSV20")] %>% rowMeans()
df_DLT_means_classified$SPRO <- df_DLT_HRVPP_classified[,c("SPRO17","SPRO18","SPRO19","SPRO20")] %>% rowMeans()
df_DLT_means_classified$TPRO <- df_DLT_HRVPP_classified[,c("TPRO17","TPRO18","TPRO19","TPRO20")] %>% rowMeans()
df_DLT_means_classified$AMPL <- df_DLT_HRVPP_classified[,c("AMPL17","AMPL18","AMPL19","AMPL20")] %>% rowMeans()

long_df_DLT_means_classified <- df_DLT_means_classified %>% gather(4:16, key='trait', value='value')
ggplot(aes(x=class, y=value), data=long_df_DLT_means_classified) + geom_boxplot() + facet_wrap(~trait, scales = "free_y")

