setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/Auto_v1.2/R')
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
featMapping <- cbind(rawNames, newNames)
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

k <- 60 # minimum data length for fitting a model
n <- length(dt[,1])
mae1 <- matrix(NA,n-k,12)
st <- tsp(dt[,1])[1]+(k-2)/12

for(i in 1:(n-k))
{
    xshort <- window(dt[,1], end=st + i/12)
    xnext <- window(dt[,1], start=st + (i+1)/12, end=st + (i+12)/12)
    fit1 <- auto.arima(xshort,seasonal = T)#, stepwise=FALSE, approximation=FALSE
    # fit2 <- stl(dt[,i], s.window = 'periodic')
    fcast1 <- forecast(fit1, h=12)
    mae1[i,1:length(xnext)] <- abs(fcast1[['mean']]-xnext)
}

plot(1:12, colMeans(mae1,na.rm=TRUE), type="l", col=2, xlab="horizon", ylab="MAE")
legend("topleft",legend=c("LM","ARIMA","ETS"),col=2:4,lty=1)
