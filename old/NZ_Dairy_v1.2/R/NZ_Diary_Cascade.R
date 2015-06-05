setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/NZ_Dairy_v1.2/R')
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
freq <- 7
dt <- ts(dt, freq=freq)
for(i in 1:ncol(dt)){
    dt[which(dt[,i]==0),i] <- NA
    dt[,i] <- na.interp(dt[,i], lambda = NULL)
}

### 0 Manually remove ###
kplist <- c(1,2,6,18,29,30,50,79,81,91,145,154,208,219:222)
dt <- dt[,kplist]; featMapping <- featMapping[kplist,]
# pairs(dt[,1:17],lower.panel=panel.smooth, upper.panel=panel.cor,diag.panel=panel.hist)

### 1. Zero-Variance Predictions
nzv <- nearZeroVar(dt, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]
# featMapping[149,];table(dt[,149]) #"Land area (sq. km)"
# nzv <- nearZeroVar(dt); dt <- dt[, -nzv]

### 2. Identifying Correlated Predictors
# descrCor <- cor(dt[,-1])
# summary(descrCor[upper.tri(descrCor)])
# highlyCorDescr <- findCorrelation(descrCor, cutoff = .95)
# cp <- featMapping[which(featMapping[,2] %in% colnames(dt[,-1][,highlyCorDescr])),]
# 
# nName <- c(colnames(dt[,1]), colnames(dt[,-1][,-highlyCorDescr]))
# dt <- cbind(dt[,1],dt[,-1][,-highlyCorDescr]);colnames(dt) <- nName
# descrCor2 <- cor(dt[,-1])
# summary(descrCor2[upper.tri(descrCor2)])

### 3. Linear Dependencies
# comboInfo <- findLinearCombos(dt[,-1])
# ld <- featMapping[which(featMapping[,2] %in% colnames(dt[,comboInfo$remove])),]
# featMapping[which(featMapping[,2] %in% colnames(dt[,-comboInfo$remove])),]
# dt <- dt[,-comboInfo$remove]

### 4. Centering and Scaling
#preProcValues <- preProcess(dt[,-1], method = c("center", "scale"))
#dt[,-1] <- predict(preProcValues, dt[,-1])

### 5. Multicollinearity
source('vif_func.r')
vifList <- featMapping[which(featMapping[,2] %in% vif_func(dt[,-c(1:3)])),]; print(vifList)
# finalVar <- c(colnames(dt[,1]), vifList[,2])
# dt <- dt[,which(colnames(dt) %in% finalVar)]

#####################################
### Indenpendent Predictive Power ###
#####################################
# anova(lmFit); require(MASS)
# step <- stepAIC(lmFit, direction="both",steps=1000)
# step$anova 
### 0. Target variable - Production ~ Cows + Livestock production index + Food production index
lmFit <- lm(Var_1~Var_2 + Var_29 + Var_79 + Var_81 + Var_145 + Var_154 + Var_219,data=dt);summary(lmFit);predict(lmFit, dt)
### 1. Cows ~ Var_6 + Var_18 + Var_50 + Var_81 + Var_145 + Var_208 + Var_219
lmFit1 <- lm(Var_2~Var_91 + Var_208 + Var_220 + Var_221,data=dt[,-c(1,5,8,9,11,12,14)]);summary(lmFit);predict(lmFit, dt)
### 2. Agri materials imports ~ Var_18 + Var_81 + Var_145 + Var_208
lmFit2 <- lm(Var_29~Var_91 + Var_208 + Var_220 + Var_221,data=dt[,-c(1,2,8,9,11,12,14)]);summary(lmFit);predict(lmFit, dt)
### 3. Food prod index ~ Var_81 + Var_91 + Var_145 + Var_208
lmFit3 <- lm(Var_79~Var_6 + Var_30 + Var_91 + Var_220 + Var_221,data=dt[,-c(1,2,5,9,11,12,14)]);summary(lmFit);predict(lmFit, dt)
### 4. Foreigh direct invest ~ 
# lmFit <- lm(Var_81~.,data=dt[,-c(1,2,5,8,11,12,14)]);summary(lmFit);predict(lmFit, dt)
### 5. Industry, value add ~ 
lmFit5 <- lm(Var_145~Var_6 + Var_30 + Var_91 + Var_208 + Var_220,data=dt[,-c(1,2,5,8,9,12,14)]);summary(lmFit);predict(lmFit, dt)
### 6. Livestock prod index ~ 
lmFit6 <- lm(Var_154~Var_6 + Var_30 + Var_91 + Var_220 + Var_221,data=dt[,-c(1,2,5,8,9,11,14)]);summary(lmFit);predict(lmFit, dt)
### 7. Wholesale price index ~ 
lmFit7 <- lm(Var_219~Var_6 + Var_30 + Var_208 + Var_220 + Var_221,data=dt[,-c(1,2,5,8,9,11,12)]);summary(lmFit);predict(lmFit, dt)

##########################
### Durbin-Watson Test ###
##########################
require(car)
durbinWatsonTest(lmFit7)

################################
### Time Series for Features ###
################################
### parameter ###
period = freq * 2
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
    jpeg(paste0('../Img/NZ_Dairy_varPred/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_var_pred.jpg'))
    plot(p,main = finFeatList[i], xaxt='n')
    axis(side=1,at=1:length(c(seq(1980, 2014+period, by = freq))),label=c(seq(1980, 2014+period, by = freq)))
    dev.off()
    jpeg(paste0('../Img/NZ_Dairy_stl/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_stl_diag.jpg'))
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

pred_mid <- matrix(nrow = period, ncol = 7)
pred_mid_80_u <- matrix(nrow = period, ncol = 7)
pred_mid_80_l <- matrix(nrow = period, ncol = 7)
pred_mid_95_u <- matrix(nrow = period, ncol = 7)
pred_mid_95_l <- matrix(nrow = period, ncol = 7)

lmFitList <- list(lmFit1, lmFit2, lmFit3, NULL, lmFit5, lmFit6, lmFit7)
for (i in 1:ncol(pred_mid)){
    if(i == 4){
        pred_mid[,i] <- pred[,8]
        pred_mid_80_u[,i] <- pred_u_80[,8]
        pred_mid_80_l[,i] <- pred_l_80[,8]
        pred_mid_95_u[,i] <- pred_u_95[,8]
        pred_mid_95_l[,i] <- pred_l_95[,8]
    }else{
        pred_mid[,i] <- predict(lmFitList[[i]], as.data.frame(pred))
        pred_mid_80_u[,i] <- predict(lmFitList[[i]], as.data.frame(pred))
        pred_mid_80_l[,i] <- predict(lmFitList[[i]], as.data.frame(pred))
        pred_mid_95_u[,i] <- predict(lmFitList[[i]], as.data.frame(pred))
        pred_mid_95_l[,i] <- predict(lmFitList[[i]], as.data.frame(pred))
    }
    
}
colnames(pred_mid) <- c('Var_2','Var_29','Var_79','Var_81','Var_145','Var_154','Var_219')
colnames(pred_mid_80_u) <- c('Var_2','Var_29','Var_79','Var_81','Var_145','Var_154','Var_219');
colnames(pred_mid_80_l) <- c('Var_2','Var_29','Var_79','Var_81','Var_145','Var_154','Var_219')
colnames(pred_mid_95_u) <- c('Var_2','Var_29','Var_79','Var_81','Var_145','Var_154','Var_219');
colnames(pred_mid_95_l) <- c('Var_2','Var_29','Var_79','Var_81','Var_145','Var_154','Var_219')

forecast <- predict(object = lmFit, newdata=as.data.frame(pred_mid))
forecast_u_95 <- predict(object = lmFit, newdata=as.data.frame(pred_mid_95_u));forecast_l_95 <- predict(object = lmFit, newdata=as.data.frame(pred_mid_95_l))
forecast_u_80 <- predict(object = lmFit, newdata=as.data.frame(pred_mid_80_u));forecast_l_80 <- predict(object = lmFit, newdata=as.data.frame(pred_mid_80_l))
# forecast <- predict(object = lmFit, newdata=as.data.frame(pred))
# forecast_u_95 <- predict(object = lmFit, newdata=as.data.frame(pred_u_95));forecast_l_95 <- predict(object = lmFit, newdata=as.data.frame(pred_l_95))
# forecast_u_80 <- predict(object = lmFit, newdata=as.data.frame(pred_u_80));forecast_l_80 <- predict(object = lmFit, newdata=as.data.frame(pred_l_80))

jpeg(paste0('../Img/NZ_Dairy_Forecasting/Production_forecasting_next',period,'Yrs.jpg'))
plot(c(dt[,1],rep(NA,length(forecast))),ylim=c(min(c(dt[,1],forecast_l_95)),max(c(dt[,1],forecast_u_95))),type = 'l',xaxt='n');
lines(c(dt[,1],forecast_u_80), col='orange');lines(c(dt[,1],forecast_l_80), col='orange');lines(c(dt[,1],forecast_u_95), col='blue');lines(c(dt[,1],forecast_l_95), col='blue')
lines(c(rep(NA,length(dt[,1])),forecast), col='red')
axis(side=1,at=1:length(timeLine),label=timeLine)
dev.off()

diff(forecast)

#############################
### Output Results to CSV ###
#############################
source('lmOut.R')
lmOut(lmFit, file=paste0('../Doc/lmFit.csv'))
for (i in 1:ncol(pred_mid)){
    if(i != 4) lmOut(lmFitList[[i]], file=paste0('../Doc/lmFit',i,'.csv'))
}

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
write.csv(dt_pred, file='../Doc/NZ_Dairy_model_predictions_full.csv', row.names=T)
