setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/R/')
require(data.table)
dt <- fread('../Auto_Sector.csv', data.table=F)
dt <- dt[,-which(names(dt) %in% c('Date'))]

require(vars)
dt2 <- ts(dt, start = c(1994,3), freq=12)
# plot.ts(dt2)
plot(dt2, plot.type='single', lty=1:29)

### var ###
library(vars)
colnames(dt2)
z=VAR(dt3[,19:20], p=12, type='both',season=12, lag.max=NULL, ic='AIC', exogen=NULL)
zp=predict(z,n.ahead =12,ci = 0.95) 
plot(zp)

### varx ###
library(dse)
SOI<- TSdata(input= dt2[,1:25],output= dt2[,26])
SOI <-tframed(SOI,list(start=c(1994,3), freq=12))
seriesNamesInput(SOI) <- colnames(dt2[,1:25])
seriesNamesOutput(SOI) <- colnames(dt2[,26])

SOI.ls<-estVARXls(window(SOI,end=c(2014,12)),max.lag=2)##
print(SOI.ls)
stability(SOI.ls)
S.p = forecast(SOI.ls, conditioning.inputs = SOI$input)
S.p$forecast
tfplot(S.p)
# rr=checkResiduals(SOI.ls)
# par(mfrow=c(1,2)) ;acf(rr$re) ;pacf(rr$re)

### forecast ###
require(forecast)
fit <- auto.arima(dt3[,19], D=12,max.P = 5, max.Q = 5);print(fit)
fit <- arima(dt3[,19], seasonal=list(order = c(2, 1, 2), period = 12))
plot(forecast(fit,h=12))
dt3 <- dt2
for (i in 2:ncol(dt3)){ # impute null value
    dt3[,i] <- na.interp(dt3[,i], lambda = NULL)
}
write.csv(dt3, file='../Auto_Sector_comp.csv', row.names=F, quote=F)

### lm models ###
fit <- lm(dt3[,19]~dt3[,c(1:18,20:29)],data=dt3)
plot(dt3, plot.type='single', lty=1)
