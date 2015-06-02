setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/NZ_Dairy_v1.2/R')
rm(list=ls());gc();source('pairs.r')
require(data.table);require(forecast);require(caret);require(bit64)
dt <- fread('../data/WorldBankData-NZ-AU-v1.csv', data.table=F, na.strings = '')
timeLine <- dt[,'Indicators']
dt <- dt[,-which(names(dt) %in% c('Indicators','Increase'))]
rawNames <- colnames(dt)
newNames <- paste0('Var_', 1:ncol(dt))
featMapping <- cbind(rawNames, newNames)
# write.csv(featMapping, file='../All_feat_mapping_NZ.csv')
colnames(dt) <- newNames
for(i in 1:ncol(dt)){
    if(class(dt[,i]) == "integer64"){dt[,i]<-as.numeric(dt[,i]/1000000)}
}

######################
### Pre-processing ###
######################
### 0. Remove 0 values and impute with predicted trend data
freq <- 7
dt <- ts(dt, freq=freq)
for(i in 1:ncol(dt)){
    dt[which(dt[,i]==0),i] <- NA
    dt[,i] <- na.interp(dt[,i], lambda = NULL)
}
