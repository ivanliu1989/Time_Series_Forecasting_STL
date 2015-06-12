require(tseries);library(fUnitRoots);library(xts)
#（1）根据趋势定差分
plot(dt[,1],type="b") #查看图像总体趋势，确定如何差分
df1 = diff(dt[,1])  #d=1阶差分
s4_df1=diff(df1,4)  #对d=1阶差分结果进行k=4步（季节）差分

#（2）根据所定差分检验平稳
adfTest(s4_df1,lag=6) #对差分结果进行平稳性检验

#（3）ARIMA(p,d,q)中的pq定阶
acf(s4_df1)
pacf(s4_df1)

#（4）建立arima模型
ans=arima(dt[,1],order=c(4,1,0),seasonal=list(order=c(1,0,1),period=4),include.mean=F,fixed=c(NA,0,0,NA,NA,NA))

#（5）检验模型残差白噪声
#//use natural log of T (the number ofobservations) which provides higher power (1 -Beta)
Box.test(s4_df1,lag=5,type='Ljung')
Box.test(ans$residuals,lag=5,type='Ljung')
#或者
tsdiag(ans)

#（6）预测
predict(ans,10)
