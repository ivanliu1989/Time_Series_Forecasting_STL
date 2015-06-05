setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/Auto_v1.3/R')
rm(list=ls());gc();source('pairs.r')
require(data.table);require(forecast);require(caret);require(bit64)
dt <- fread('../data/Auto_Sector_Growth.csv', data.table=F, na.strings = '')
dt2 <- fread('../data/Auto_Sector.csv', data.table=F, na.strings = '')
dt2 <- dt
####################
### Convert Data ###
####################
timeLine <- dt[,'Date']
dt[,1] <- NULL; dt2[,1] <- NULL# 1994/04/01 
dt <- dt[,c(1:18,20:29,19)];dt2 <- dt2[,c(1:18,20:29,19)]
rawNames <- colnames(dt);newNames <- c(paste0('Var_', 1:(ncol(dt)-1)), 'Target')
featMapping <- cbind(rawNames, newNames)
write.csv(featMapping, file='../Document/Feature_Mapping.csv')
colnames(dt) <- newNames

######################
### Pre-processing ###
######################
### 0. Remove 0 values and impute with predicted trend data
freq <- 12
dt <- ts(dt, freq=freq, start = c(1994,4));dt2 <- ts(dt2, freq=freq, start = c(1994,4))
for(i in 1:ncol(dt)){
    dt[which(dt[,i]==0),i] <- NA
    dt[,i] <- na.interp(dt[,i], lambda = NULL)
}
for(i in 1:ncol(dt2)){
    dt2[which(dt2[,i]==0),i] <- NA
    dt2[,i] <- na.interp(dt2[,i], lambda = NULL)
}
write.csv(dt2, file='../data/Datasets_imputed.csv')

################################
### Time Series for Features ###
################################
### parameter ###
period = 2 * freq
### models ###
pred <- matrix(nrow = period, ncol = (ncol(dt)-1))
pred_u_95 <- matrix(nrow = period, ncol = (ncol(dt)-1));pred_l_95 <- matrix(nrow = period, ncol = (ncol(dt)-1))
pred_u_80 <- matrix(nrow = period, ncol = (ncol(dt)-1));pred_l_80 <- matrix(nrow = period, ncol = (ncol(dt)-1))
finFeatList <- featMapping[which(featMapping[,2] %in% colnames(dt)),][,1]
modelInfo <- 
    for(i in 1:(ncol(dt)-1)){
        fit <- auto.arima(dt[,i],seasonal = T)#, stepwise=FALSE, approximation=FALSE
        fit2 <- stl(dt[,i], s.window = 'periodic')
        p <- forecast(fit, h=period)
        
        jpeg(paste0('../Image/Forecasting_Predictors/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_var_pred.jpg'),width=600, height=400)
        plot(p,main = finFeatList[i])
        dev.off()
        jpeg(paste0('../Image/STL_diagram/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_stl.jpg'),width=600, height=400)
        plot(fit2,main = finFeatList[i])
        dev.off()
        jpeg(paste0('../Image/ACF_PACF/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_acf_pacf.jpg'),width=600, height=400)
        #     par(mfcol = c(2,1));acf(dt[,i]);pacf(dt[,i]);par(mfcol = c(1,1))
        tsdisplay(dt[,i],main = finFeatList[i])
        dev.off()
        jpeg(paste0('../Image/ACF_PACF_Difference/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_acf_pacf_diff.jpg'),width=600, height=400)
        tsdisplay(diff(diff(dt[,i],12),12),main = finFeatList[i])
        dev.off()
        
        pred[,i] <- as.data.frame(p)[,1]
        pred_u_95[,i] <- as.data.frame(p)[,5]
        pred_l_95[,i] <- as.data.frame(p)[,4]
        pred_u_80[,i] <- as.data.frame(p)[,3]
        pred_l_80[,i] <- as.data.frame(p)[,2]
    }

colnames(pred) <- colnames(dt)[1:(ncol(dt)-1)]
fit <- auto.arima(dt[,29], xreg = dt[,-29])#, stepwise=FALSE, approximation=FALSE
write.csv(fit$coef, file='../Document/Fit_Coef.csv', row.names = F)
p <- forecast(fit, h=period, xreg=pred)
p_u_95 <- forecast(fit, h=period, xreg=pred_u_95);p_l_95 <- forecast(fit, h=period, xreg=pred_l_95)
p_u_80 <- forecast(fit, h=period, xreg=pred_u_80);p_l_80 <- forecast(fit, h=period, xreg=pred_l_80)

jpeg(paste0('../Image/Forecasting_Target/Production_forecasting_multi',period,'months.jpg'),width=600, height=400)
plot(c(dt[,29],rep(NA,length(p$mean))),type = 'l', xaxt='n', ylab='Sales Volume', ylim=c(min(dt2[,29],p_u_95$lower[,2]),max(dt2[,29],p_u_95$upper[,2])));
lines(c(dt[,29],p_u_80$mean), col='orange');lines(c(dt[,29],p_l_80$mean), col='orange');
lines(c(dt[,29],p_u_95$mean), col='blue');lines(c(dt[,29],p_l_95$mean), col='blue')
lines(c(rep(NA,length(dt[,29])),p$mean), col='red')
axis(side=1,at=1:length(timeLine),label=timeLine)
dev.off()

#############
### Other ###
#############
# acf(dt[,29])
# pacf(dt[,29])
# seasonplot(dt[,29])
# accuracy(fit)
# monthplot(dt[,29])
# HoltWinters(dt[,29])

###################
### Correlation ###
###################
cor_no_lag <- cor(dt)
write.csv(cor_no_lag, file='../Document/Correlation_No_Lag.csv')

lags <- 5*12
cffdf <- matrix(nrow = lags*2+1, ncol = (ncol(dt)+1)) 
for(i in 1:ncol(dt)){
    jpeg(paste0('../Image/Lagged_Predictors/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_lag_cor.jpg'),width=600, height=400)
    x <- dt[,i]; y <- dt[,29]
    diff1x=x#diff(x,12)
    ar1model = auto.arima(x)
    pwx=ar1model$residuals
    ar2model = auto.arima(y)
    newpwy=ar2model$residuals
    # newpwy = filter(y, filter = c(1,-(1+ar1model$coef),ar1model$coef), sides =1)
    cffvalues <- ccf(pwx,newpwy,na.action=na.omit,lag.max=lags, plot=T,ylab = "cross-correlation",
                     main=paste0(finFeatList[i]," & New Vehicle Sales"))
    cffdf[,1] <- cffvalues$lag; cffdf[,(i+1)] <- cffvalues$acf
    dev.off()
}
colnames(cffdf) <- c('LagNum', rawNames)
write.csv(cffdf, file='../Document/Correlation_Lagged.csv', row.names = F)

for(i in 1:ncol(dt)){
    jpeg(paste0('../Image/Lagged_Predictors_Raw/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_lag_cor.jpg'),width=600, height=400)
    x <- dt[,i]; y <- dt[,29]
    cffvalues <- ccf(x,y,na.action=na.omit,lag.max=lags, plot=T,ylab = "cross-correlation",
                     main=paste0(finFeatList[i]," & New Vehicle Sales"))
    cffdf[,1] <- cffvalues$lag; cffdf[,(i+1)] <- cffvalues$acf
    dev.off()
}
colnames(cffdf) <- c('LagNum', rawNames)
write.csv(cffdf, file='../Document/Correlation_Lagged_Raw.csv', row.names = F)

### Choose Lag ###
Find_Max_CCF<- function(x,y, lags) { 
    #diff1x=diff(x,12)
    ar1model = auto.arima(x)
    pwx=ar1model$residuals
    ar2model = auto.arima(y)
    newpwy=ar2model$residuals
    #newpwy = filter(y, filter = c(1,-(1+ar1model$coef),ar1model$coef), sides =1)
    d <- ccf(x,y,na.action=na.omit,lag.max=lags, plot=F,ylab = "cross-correlation",
             main=paste0(finFeatList[i]," & New Vehicle Sales"))
    upperCI <- qnorm((1 + 0.95)/2)/sqrt(d$n.used)
    lowerCI <- -qnorm((1 + 0.95)/2)/sqrt(d$n.used)
    cor = d$acf[,,1] 
    lag = d$lag[,,1] 
    res = data.frame(cor,lag) 
    if(abs(res[which.max(abs(res$cor)),1])>=abs(upperCI)){
        res_max = res[which.max(abs(res$cor)),]
        return(res_max) 
    }else{
        res_max = res[which(res[,2]==0),]
        return(res_max)
    }
}

cffmat <- matrix(nrow = 28, ncol = 3) 
for(i in 1:28){
    feat_name <- finFeatList[i]
    lag_imp <- Find_Max_CCF(dt[,i], dt[,29], lags)
    lag_period <- lag_imp[,2]/(1/12)
    lag_cor <- lag_imp[,1]
    cffmat[i,1]<-feat_name; cffmat[i,2]<-lag_period; cffmat[i,3]<-lag_cor
}
colnames(cffmat) <- c('Features', 'Lag', 'Correlation')
write.csv(cffmat, file='../Document/Lag_Impacts_Raw.csv', row.names = F)

###############################
### Convert to Real Numbers ###
###############################
# stPoint <- dt2[,29][254]
# p_real <- matrix(nrow = length(p$mean), ncol = 5) 
# for(j in 1:5){
#     for(i in 1:length(p$mean)){
#         if(i == 1){
#             p_real[i,j] <- stPoint * (1 + as.data.frame(p)[i,j])
#         }else{
#             p_real[i,j] <- p_real[(i-1),j] * (1 + as.data.frame(p)[i,j])    
#         }
#     } 
# }
# jpeg(paste0('../Image/forecasting/Production_forecasting_multi',period,'months_real.jpg'))
# plot(c(dt2[,29],rep(NA,length(p_real[,1]))),type = 'l', xaxt='n', ylab='Sales Volume', ylim=c(min(dt2[,29],p_real[,4]),max(dt2[,29],p_real[,5])));
# lines(c(dt2[,29],p_real[,3]), col='orange');lines(c(dt2[,29],p_real[,2]), col='orange');
# lines(c(dt2[,29],p_real[,5]), col='blue');lines(c(dt2[,29],p_real[,4]), col='blue')
# lines(c(rep(NA,length(dt2[,29])),p_real[,1]), col='red')
# axis(side=1,at=1:length(timeLine),label=timeLine)
# dev.off()