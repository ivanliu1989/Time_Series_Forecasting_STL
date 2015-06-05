setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/Auto_v1.3/R')
rm(list=ls());gc();source('pairs.r')
require(data.table);require(forecast);require(caret);require(bit64)
dt <- fread('../data/Auto_Sector.csv', data.table=F, na.strings = '')

####################
### Convert Data ###
####################
timeLine <- dt[,'Date']
dt[,1] <- NULL# 1994/04/01 
dt <- dt[,c(1:18,20:29,19)]
rawNames <- colnames(dt);newNames <- c(paste0('Var_', 1:(ncol(dt)-1)), 'Target')
nullRate <- apply(dt,2, function(x) mean(is.na(x)))
featMapping <- cbind(rawNames, newNames, nullRate)
write.csv(featMapping, file='../Document/Feature_Mapping.csv')
colnames(dt) <- newNames

######################
### Pre-processing ###
######################
### 0. Remove 0 values and impute with predicted trend data
freq <- 12
dt <- ts(dt, freq=freq, start = c(1994,4))
for(i in 1:ncol(dt)){
    dt[which(dt[,i]==0),i] <- NA
    dt[,i] <- na.interp(dt[,i], lambda = NULL)
}

################################
### Time Series for Features ###
################################
### parameter ###
period = 60
train = 1:(nrow(dt)-period)
test = (nrow(dt)-period+1):nrow(dt)

### models ###
Error <- matrix(nrow = ncol(dt), ncol = 4, dimnames = list(NULL, c('Features','MeanError','80Error','95Error')))
finFeatList <- featMapping[which(featMapping[,2] %in% colnames(dt)),][,1]
for(i in 1:(ncol(dt)-1)){
    fit <- auto.arima(dt[train,i],seasonal = T, stepwise=FALSE, approximation=FALSE)#
    p <- forecast(fit, h=period)
    
    jpeg(paste0('../Image/Forecasting_Predictors_Validation/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_validation.jpg'),width=600, height=400)
    plot(c(dt[,i]),type = 'l', xaxt='n',main = paste0(finFeatList[i],' | Null Value: ',as.numeric(featMapping[i,3])*100, '%'),ylim=c(min(dt[,i],p$lower[,2]),max(dt[,i],p$upper[,2])), ylab=finFeatList[i]);
    lines(c(rep(NA,length(dt[train,i])),p$lower[,2]), col='blue');lines(c(rep(NA,length(dt[train,i])),p$lower[,1]), col='orange');
    lines(c(rep(NA,length(dt[train,i])),p$upper[,2]), col='blue');lines(c(rep(NA,length(dt[train,i])),p$upper[,1]), col='orange')
    lines(c(rep(NA,length(dt[train,i])),p$mean), col='red')
    axis(side=1,at=1:length(timeLine),label=timeLine)
    dev.off()
    Error[i,1] <- finFeatList[i]
    Error[i,2] <- mean(abs(p$mean - dt[test,i])/abs(dt[test,i]))*100
    Error[i,3] <- mean(abs(apply(cbind(apply(cbind(p$upper[,1]-dt[test,i], 0),1,min),apply(cbind(dt[test,i]-p$lower[,1], 0),1,min)),1,min))/abs(dt[test,i]))*100
    Error[i,4] <- mean(abs(apply(cbind(apply(cbind(p$upper[,2]-dt[test,i], 0),1,min),apply(cbind(dt[test,i]-p$lower[,2], 0),1,min)),1,min))/abs(dt[test,i]))*100
}

i <- 29
fit <- auto.arima(dt[train,29], xreg = dt[train,-c(7,8,27,29)], stepwise=FALSE, approximation=FALSE)#
p <- forecast(fit, h=period, xreg=dt[test,-c(7,8,27,29)])
jpeg(paste0('../Image/Forecasting_Target_Validation/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_validation.jpg'),width=600, height=400)
plot(c(dt[,i]),type = 'l', xaxt='n',main = finFeatList[i],ylim=c(min(dt[,i],p$lower[,2]),max(dt[,i],p$upper[,2])), ylab=finFeatList[i]);
lines(c(rep(NA,length(dt[train,i])),p$lower[,2]), col='blue');lines(c(rep(NA,length(dt[train,i])),p$lower[,1]), col='orange');
lines(c(rep(NA,length(dt[train,i])),p$upper[,2]), col='blue');lines(c(rep(NA,length(dt[train,i])),p$upper[,1]), col='orange')
lines(c(rep(NA,length(dt[train,i])),p$mean), col='red')
axis(side=1,at=1:length(timeLine),label=timeLine)
dev.off()

Error[i,1] <- finFeatList[i]
Error[i,2] <- mean(abs(p$mean - dt[test,i])/abs(dt[test,i]))*100
Error[i,3] <- mean(abs(apply(cbind(apply(cbind(p$upper[,1]-dt[test,i], 0),1,min),apply(cbind(dt[test,i]-p$lower[,1], 0),1,min)),1,min))/abs(dt[test,i]))*100
Error[i,4] <- mean(abs(apply(cbind(apply(cbind(p$upper[,2]-dt[test,i], 0),1,min),apply(cbind(dt[test,i]-p$lower[,2], 0),1,min)),1,min))/abs(dt[test,i]))*100

write.csv(Error, file='../Document/Prediction_Error.csv', row.names = F)
