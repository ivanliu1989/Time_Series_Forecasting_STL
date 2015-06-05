setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/Time_Series_Forecasting_STL/R/')
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
# pairs(dt[,1:8],lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist)
# rawNames[c(1:7,9:10)]

### parameters ###
period <- 15
timeLine <- c(min(timeLine):(max(timeLine)+period))
xmin <- min(timeLine); xmax <- max(timeLine)
### models ###
pred <- matrix(nrow = period, ncol = (ncol(dt)-1))
pred_u_95 <- matrix(nrow = period, ncol = (ncol(dt)-1));pred_l_95 <- matrix(nrow = period, ncol = (ncol(dt)-1))
pred_u_80 <- matrix(nrow = period, ncol = (ncol(dt)-1));pred_l_80 <- matrix(nrow = period, ncol = (ncol(dt)-1))
finFeatList <- featMapping[which(featMapping[,2] %in% colnames(dt)),][,1]

for(i in 2:ncol(dt)){
    fit <- stl(dt[,i], s.window = 'periodic');p <- forecast(fit,h=period)
#     p <- stlf(dt[,i], lambda=BoxCox.lambda(dt[,i]),h = period)
    
    jpeg(paste0('../image/NZ_Dairy_varPred_ALL/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_var_pred.jpg'))
    plot(p,main = finFeatList[i], xaxt='n')
    axis(side=1,at=1:length(c(seq(1980, 2014+period, by = freq))),label=c(seq(1980, 2014+period, by = freq)))
    dev.off()
    jpeg(paste0('../image/NZ_Dairy_stl_ALL/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_stl_diag.jpg'))
    plot(fit,main = finFeatList[i], xaxt='n')
    axis(side=1,at=1:length(c(seq(1980, 2014+period, by = freq))),label=c(seq(1980, 2014+period, by = freq)))
    dev.off()
    
    pred[,(i-1)] <- as.data.frame(p)[,1]
    pred_u_95[,(i-1)] <- as.data.frame(p)[,5]
    pred_l_95[,(i-1)] <- as.data.frame(p)[,4]
    pred_u_80[,(i-1)] <- as.data.frame(p)[,3]
    pred_l_80[,(i-1)] <- as.data.frame(p)[,2]
}
pred_full <- pred

### 0 Manually remove ###
list <- as.matrix(fread('../data/NZ_Feature_num.csv', data.table=F))
incFeat <- paste0('Var_',list)
dt <- dt[,which(colnames(dt) %in% incFeat)]

### 1. Zero-Variance Predictions
nzv <- nearZeroVar(dt, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]
# featMapping[149,];table(dt[,149]) #"Land area (sq. km)"
nzv <- nearZeroVar(dt); dt <- dt[, -nzv]

### 2. Identifying Correlated Predictors
descrCor <- cor(dt[,-c(1:6)])
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .99)
cp <- featMapping[which(featMapping[,2] %in% colnames(dt[,-c(1:6)][,highlyCorDescr])),]
write.csv(cp, file='../NZ_Diary_doc/correlated_pred_NZ.csv')

nName <- c(colnames(dt[,c(1:6)]), colnames(dt[,-c(1:6)][,-highlyCorDescr]))
dt <- cbind(dt[,c(1:6)],dt[,-c(1:6)][,-highlyCorDescr]);colnames(dt) <- nName
descrCor2 <- cor(dt[,-c(1:6)])
summary(descrCor2[upper.tri(descrCor2)])

### 3. Linear Dependencies
comboInfo <- findLinearCombos(dt[,-1])
ld <- featMapping[which(featMapping[,2] %in% colnames(dt[,comboInfo$remove])),]
write.csv(ld, file='../NZ_Diary_doc/linear_dependency_NZ.csv')
featMapping[which(featMapping[,2] %in% colnames(dt[,-comboInfo$remove])),]
dt <- dt[,-comboInfo$remove]
# write.csv(featMapping[comboInfo$remove,], file='../linearDependency_NZ.csv')

### 4. Centering and Scaling
preProcValues <- preProcess(dt[,-1], method = c("center", "scale"))
dt[,-1] <- predict(preProcValues, dt[,-1])

### 5. Multicollinearity
source('vif_func.r')
vifList <- featMapping[which(featMapping[,2] %in% vif_func(dt[,-c(1:6)])),]
write.csv(vifList, file='../NZ_Diary_doc/multicollinearity_NZ.csv')

finalVar <- c(colnames(dt[,c(1:6)]), vifList[,2])
dt <- dt[,which(colnames(dt) %in% finalVar)]

#################################
### Model for Target Variable ###
#################################
lmFit <- lm(Var_1~.,data=dt)
# summary(lmFit)
# featMapping[c(9,219),];cor(dt[,c(9,219)])

################################
### Time Series for Features ###
################################
### models ###
pred <- matrix(nrow = period, ncol = (ncol(dt)-1))
pred_u_95 <- matrix(nrow = period, ncol = (ncol(dt)-1));pred_l_95 <- matrix(nrow = period, ncol = (ncol(dt)-1))
pred_u_80 <- matrix(nrow = period, ncol = (ncol(dt)-1));pred_l_80 <- matrix(nrow = period, ncol = (ncol(dt)-1))
finFeatList <- featMapping[which(featMapping[,2] %in% colnames(dt)),][,1]

for(i in 2:ncol(dt)){
    fit <- stl(dt[,i], s.window = 'periodic');p <- forecast(fit,h=period)
#     p <- stlf(dt[,i], lambda=BoxCox.lambda(dt[,i]),h = period)
    jpeg(paste0('../image/NZ_Dairy_varPred/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_var_pred.jpg'))
    plot(p,main = finFeatList[i], xaxt='n')
    axis(side=1,at=1:length(c(seq(1980, 2014+period, by = freq))),label=c(seq(1980, 2014+period, by = freq)))
    dev.off()
    jpeg(paste0('../image/NZ_Dairy_stl/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_stl_diag.jpg'))
    plot(fit,main = finFeatList[i], xaxt='n')
    axis(side=1,at=1:length(c(seq(1980, 2014+period, by = freq))),label=c(seq(1980, 2014+period, by = freq)))
    dev.off()
    
    pred[,(i-1)] <- as.data.frame(p)[,1]
    pred_u_95[,(i-1)] <- as.data.frame(p)[,5]
    pred_l_95[,(i-1)] <- as.data.frame(p)[,4]
    pred_u_80[,(i-1)] <- as.data.frame(p)[,3]
    pred_l_80[,(i-1)] <- as.data.frame(p)[,2]
}

###################
### Predictions ###
###################
colnames(pred) <- colnames(dt)[2:ncol(dt)]
colnames(pred_u_80) <- colnames(dt)[2:ncol(dt)];colnames(pred_l_80) <- colnames(dt)[2:ncol(dt)]
colnames(pred_u_95) <- colnames(dt)[2:ncol(dt)];colnames(pred_l_95) <- colnames(dt)[2:ncol(dt)]
forecast <- predict(object = lmFit, newdata=as.data.frame(pred))
forecast_u_95 <- predict(object = lmFit, newdata=as.data.frame(pred_u_95));forecast_l_95 <- predict(object = lmFit, newdata=as.data.frame(pred_l_95))
forecast_u_80 <- predict(object = lmFit, newdata=as.data.frame(pred_u_80));forecast_l_80 <- predict(object = lmFit, newdata=as.data.frame(pred_l_80))

jpeg(paste0('../image/NZ_Dairy_Forecasting/Production_forecasting_next',period,'Yrs.jpg'))
plot(c(dt[,1],rep(NA,length(forecast))),ylim=c(min(c(dt[,1],forecast_l_95)),max(c(dt[,1],forecast_u_95))),type = 'l',xaxt='n');
lines(c(dt[,1],forecast_u_80), col='orange');lines(c(dt[,1],forecast_l_80), col='orange');lines(c(dt[,1],forecast_u_95), col='blue');lines(c(dt[,1],forecast_l_95), col='blue')
lines(c(rep(NA,length(dt[,1])),forecast), col='red')
axis(side=1,at=1:length(timeLine),label=timeLine)
dev.off()

diff(forecast)

modelExp <- lmFit$coefficients
modelExp <- as.data.frame(cbind(c('Intercept',finFeatList[-1]),modelExp))
print(modelExp)
write.csv(modelExp, file='../NZ_Dairy_model.csv', row.names=F, quote=F)

t_pred <- cbind(forecast, pred_full); colnames(t_pred) <- rawNames
dt <- fread('../data/WorldBankData-NZ-AU-v1.csv', data.table=F, na.strings = '')
dt <- dt[,-which(names(dt) %in% c('Indicators','Increase'))]
for(i in 1:ncol(dt)){
    if(class(dt[,i]) == "integer64"){dt[,i]<-as.numeric(dt[,i]/1000000)}
}
dt <- ts(dt, start = c(1980), freq=7)
for(i in 1:ncol(dt)){
    dt[which(dt[,i]==0),i] <- NA
    dt[,i] <- na.interp(dt[,i], lambda = NULL)
}
dt_pred <- rbind(dt, t_pred)
write.csv(dt_pred, file='../NZ_Diary_doc/NZ_Dairy_model_predictions_full.csv', row.names=T)
