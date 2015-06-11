### Prewhitening Method One ### 
x <- dt[,i];y <- dt[,29]
#ccf(x,y, na.action = na.omit);acf(x)
diff1x=x#diff(x,12)
#acf(diff1x, na.action = na.omit)
#pacf(diff1x, na.action = na.omit)
ar1model = auto.arima(diff1x)
ar1model
pwx=ar1model$residuals
newpwy = filter(y, filter = c(1,-(1+ar1model$coef),ar1model$coef), sides =1)
ccf(pwx,newpwy,na.action=na.omit,lag.max=lags)

### Prewhitening Method Two ### 
x <- dt[,i];y <- dt[,29]
#ccf(x,y, na.action = na.omit);acf(x)
diff1x=x#diff(x,12)
diff1y=y#diff(y,12)
#acf(diff1x, na.action = na.omit)
#pacf(diff1x, na.action = na.omit)
ar1model = auto.arima(diff1x)
ar1model
pwx=ar1model$residuals
ar2model = auto.arima(diff1y)
ar2model
newpwy=ar2model$residuals
ccf(pwx,newpwy,na.action=na.omit,lag.max=lags)
