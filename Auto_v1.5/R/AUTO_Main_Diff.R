acf(diff(diff(dt2[,29],12),12),lag.max = 600)
acf(diff(dt2[,29],12),lag.max = 600)
acf(dt2[,29],lag.max = 600)
acf(diff(diff(dt2[,29],4),4),lag.max = 600)
acf(diff(dt2[,29],4),lag.max = 600)

cffvalues <- ccf(diff(dt[,2],12), diff(dt[,29],12), plot=TRUE, lag.max=60)

tsdisplay(diff(dt2[,29],12))
tsdisplay(diff(diff(dt2[,29],12),12))
tsdisplay(dt2[,29])

tsdisplay(residuals(fit))
Box.test(residuals(fit), lag=36, fitdf=8, type="Ljung")

spectrum(data.frame(dt[,29], dt[,2]))$coh
spectrum(data.frame(dt[,29], dt[,2]))$spec

### Lagged Predictors ###
plot(dt2[,29], main="Car Sales Volume", xlab="Year")

# Lagged predictors. Test 0, 1, 2 or 3 lags.
laggedPred <- cbind(c(dt2[,25]),
                    c(NA,dt2[1:(length(dt2[,2])-1),25]),
                    c(NA,NA,dt2[1:(length(dt2[,2])-2),25]),
                    c(NA,NA,NA,dt2[1:(length(dt2[,2])-3),25]))
colnames(laggedPred) <- paste("LagPred",0:3,sep="")

# Choose optimal lag length for advertising based on AIC
# Restrict data so models use same fitting period
fit1 <- auto.arima(dt2[4:length(dt2[,29]),29], xreg=laggedPred[4:length(dt2[,29]),1])
fit2 <- auto.arima(dt2[4:length(dt2[,29]),29], xreg=laggedPred[4:length(dt2[,29]),1:2])
fit3 <- auto.arima(dt2[4:length(dt2[,29]),29], xreg=laggedPred[4:length(dt2[,29]),1:3])
fit4 <- auto.arima(dt2[4:length(dt2[,29]),29], xreg=laggedPred[4:length(dt2[,29]),1:4])

# Best model fitted to all data (based on AICc)
# Refit using all data
fit <- auto.arima(dt2[,29], xreg=laggedPred[,1])

### Choose Lag ###
Find_Max_CCF<- function(a,b, lags) { 
    d <- ccf(a, b, plot = FALSE, lag.max=lags) 
    cor = d$acf[,,1] 
    lag = d$lag[,,1] 
    res = data.frame(cor,lag) 
    res_max = res[which.max(abs(res$cor)),] 
    return(res_max) 
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
write.csv(cffmat, file='../Document/Lag_Impacts.csv', row.names = F)

### pre whitening ###
x = arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200) #<============
z = ts.intersect(x, lag(x,-3), lag(x,-4))
y = 15+0.8*z[,2]+1.5*z[,3]
ccf(z[,1],y,na.action = na.omit)

acf(x)
diff1x=diff(z[,1],1)
acf(diff1x, na.action = na.omit)
pacf(diff1x, na.action = na.omit)
ar1model = arima(z[,1], order = c(1,1,0))
ar1model
pwx=ar1model$residuals
newpwy = filter(y, filter = c(1,-(1+ar1model$coef),ar1model$coef), sides =1)
ccf (pwx,newpwy,na.action=na.omit)

### pre whitening ###
x <- dt[,i];y <- dt[,29]
#ccf(x,y, na.action = na.omit);acf(x)
diff1x=diff(x,12)
acf(diff1x, na.action = na.omit)
pacf(diff1x, na.action = na.omit)
ar1model = auto.arima(x, stepwise=FALSE, approximation=FALSE)
ar1model
pwx=ar1model$residuals
newpwy = filter(y, filter = c(1,-(1+ar1model$coef),ar1model$coef), sides =1)
ccf(pwx,newpwy,na.action=na.omit,lag.max=lags)
