setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/NZ_Dairy_v1.3/R')
rm(list=ls());gc();source('pairs.r')
require(data.table);require(forecast);require(caret);require(bit64)
dt <- fread('../data/WorldBankData-NZ-AU-v1.1.csv', data.table=F, na.strings = '')
timeLine <- dt[,'Indicators']
dt <- dt[,-which(names(dt) %in% c('Indicators','Increase'))]
rawNames <- colnames(dt);newNames <- paste0('Var_', 1:ncol(dt))
featMapping <- cbind(rawNames, newNames)
write.csv(featMapping, file='../Doc/All_feat_mapping_NZ.csv')
colnames(dt) <- newNames
for(i in 1:ncol(dt)){
    if(class(dt[,i]) == "integer64"){dt[,i]<-as.numeric(dt[,i]/1000000)}
}

######################
### Pre-processing ###
######################
### 0. Remove 0 values and impute with predicted trend data
freq <- 5
dt <- ts(dt, freq=freq)
for(i in 1:ncol(dt)){
    dt[which(dt[,i]==0),i] <- NA
    dt[,i] <- na.interp(dt[,i], lambda = NULL)
}
acf(dt[,1])
pacf(dt[,1])
### 0 Manually remove ###
kplist <- c(1:6,16,18,20,22,29,30,44,49:50,79,81,91,130,145,153,154,183,194,208,219:222)
dt <- dt[,kplist]; featMapping <- featMapping[kplist,]
# pairs(dt[,1:17],lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist)

### 1. Zero-Variance Predictions
nzv <- nearZeroVar(dt, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]
# featMapping[149,];table(dt[,149]) #"Land area (sq. km)"
# nzv <- nearZeroVar(dt); dt <- dt[, -nzv]

################################
### Time Series for Features ###
################################
### parameter ###
period = 10
timeLine <- c(min(timeLine):(max(timeLine)+period))
xmin <- min(timeLine); xmax <- max(timeLine)
### models ###
pred <- matrix(nrow = period, ncol = (ncol(dt)-1))
pred_u_95 <- matrix(nrow = period, ncol = (ncol(dt)-1));pred_l_95 <- matrix(nrow = period, ncol = (ncol(dt)-1))
pred_u_80 <- matrix(nrow = period, ncol = (ncol(dt)-1));pred_l_80 <- matrix(nrow = period, ncol = (ncol(dt)-1))
finFeatList <- featMapping[which(featMapping[,2] %in% colnames(dt)),][,1]

for(i in 2:ncol(dt)){
    fit <- auto.arima(dt[,i],seasonal = T, stepwise=FALSE, approximation=FALSE)
    p <- forecast(fit, h=period)
    fit2 <- stl(dt[,i], s.window = 'periodic')
    
    jpeg(paste0('../Img/NZ_Dairy_varPred/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_var_pred.jpg'))
    plot(p,main = finFeatList[i], xaxt='n')
    axis(side=1,at=1:length(c(seq(1980, 2014+period, by = freq))),label=c(seq(1980, 2014+period, by = freq)))
    dev.off()
    jpeg(paste0('../Img/NZ_Dairy_stl/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_stl_diag.jpg'))
    plot(fit2,main = finFeatList[i], xaxt='n')
    axis(side=1,at=1:length(c(seq(1980, 2014+period, by = freq))),label=c(seq(1980, 2014+period, by = freq)))
    dev.off()
    
    pred[,(i-1)] <- as.data.frame(p)[,1]
    pred_u_95[,(i-1)] <- as.data.frame(p)[,5]
    pred_l_95[,(i-1)] <- as.data.frame(p)[,4]
    pred_u_80[,(i-1)] <- as.data.frame(p)[,3]
    pred_l_80[,(i-1)] <- as.data.frame(p)[,2]
}

colnames(pred) <- colnames(dt)[2:ncol(dt)]
fit <- auto.arima(dt[,1], xreg = dt[,-1], stepwise=FALSE, approximation=FALSE)#
p <- forecast(fit, h=period, xreg=pred)
p_u_95 <- forecast(fit, h=period, xreg=pred_u_95);p_l_95 <- forecast(fit, h=period, xreg=pred_l_95)
p_u_80 <- forecast(fit, h=period, xreg=pred_u_80);p_l_80 <- forecast(fit, h=period, xreg=pred_l_80)

jpeg(paste0('../Img/NZ_Dairy_Forecasting/Production_forecasting_multi',period,'Yrs.jpg'))
plot(c(dt[,1],rep(NA,length(p))),ylim=c(min(c(dt[,1],p_l_95$mean)),max(c(dt[,1],p_u_95$mean))),type = 'l',xaxt='n');
lines(c(dt[,1],p_u_80$mean), col='orange');lines(c(dt[,1],p_l_80$mean), col='orange');lines(c(dt[,1],p_u_95$mean), col='blue');lines(c(dt[,1],p_l_95$mean), col='blue')
lines(c(rep(NA,length(dt[,1])),p$mean), col='red')
axis(side=1,at=1:length(timeLine),label=timeLine)
dev.off()
