setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/Time_Series_Forecasting_STL/R/')
require(data.table);require(forecast)
dt <- fread('../data/Auto_Sector_comp.csv', data.table=F)
dt <- dt[,-which(names(dt) %in% c('Date'))]
# for (i in 1:ncol(dt)){ # impute null value
#     dt[,i] <- na.interp(dt[,i], lambda = NULL)
# }

### parameters ###
period = 24 #<<===================@ input prediction periods here

### main function ###
rawNames <- colnames(dt)
dt2 <- ts(dt, start = c(1994,3), freq=12)
names <- paste0('Var', 1:29)
colnames(dt2) <- names
salesFit <- lm(Var29~.,data=dt2)
    
pred <- matrix(nrow = period, ncol = (ncol(dt)-1))
pred_u_95 <- matrix(nrow = period, ncol = (ncol(dt)-1));pred_l_95 <- matrix(nrow = period, ncol = (ncol(dt)-1))
pred_u_80 <- matrix(nrow = period, ncol = (ncol(dt)-1));pred_l_80 <- matrix(nrow = period, ncol = (ncol(dt)-1))

for(i in 1:(ncol(dt)-1)){
    #para <- auto.arima(dt[,i], D=12, max.P = 3, max.Q = 3);print(para)
    #fit <- arima(method="ML", dt[,i], seasonal=list(order = c(para$arma[1], para$arma[5], para$arma[2]), period = 12))
    #fit <- stl(ts(dt[,i],start = c(1994,3), freq=12), s.window=12)#"periodic")
    p <- stlf(ts(dt[,i],start = c(1994,3), freq=12), lambda=BoxCox.lambda(ts(dt[,i],start = c(1994,3), freq=12)),h = period)
    #p <- forecast(fit,h=period)
    #jpeg(paste0('../image/',gsub("[^[:alnum:]]", "", colnames(dt)[i]),'.jpg'))
    plot(p,main = colnames(dt[,c(i,ifelse(i==(ncol(dt)-1),1,i+1))])[1])
    #dev.off()
    
    pred[,i] <- as.data.frame(p)[,1]
    pred_u_95[,i] <- as.data.frame(p)[,5]
    pred_l_95[,i] <- as.data.frame(p)[,4]
    pred_u_80[,i] <- as.data.frame(p)[,3]
    pred_l_80[,i] <- as.data.frame(p)[,2]
}

colnames(pred) <- colnames(dt2)[1:28]
colnames(pred_u_80) <- colnames(dt2)[1:28];colnames(pred_l_80) <- colnames(dt2)[1:28]
colnames(pred_u_95) <- colnames(dt2)[1:28];colnames(pred_l_95) <- colnames(dt2)[1:28]
forecast <- predict(object = salesFit, newdata=as.data.frame(pred))
forecast_u_95 <- predict(object = salesFit, newdata=as.data.frame(pred_u_95));forecast_l_95 <- predict(object = salesFit, newdata=as.data.frame(pred_l_95))
forecast_u_80 <- predict(object = salesFit, newdata=as.data.frame(pred_u_80));forecast_l_80 <- predict(object = salesFit, newdata=as.data.frame(pred_l_80))
plot(c(dt[,29],rep(NA,length(forecast))),ylim=c(min(c(dt[,29],forecast_l_95)),max(c(dt[,29],forecast_u_95))),type = 'l');
lines(c(dt[,29],forecast_u_80), col='orange');lines(c(dt[,29],forecast_l_80), col='orange');lines(c(dt[,29],forecast_u_95), col='blue');lines(c(dt[,29],forecast_l_95), col='blue')
lines(c(rep(NA,length(dt[,29])),forecast), col='red')

# modelExp <- salesFit$coefficients
# modelExp <- as.data.frame(cbind(c('Intercept',rawNames[1:28]),modelExp))
# print(modelExp)
# write.csv(modelExp, file='../Auto_sector_model.csv', row.names=F, quote=F)
# 
# t_pred <- cbind(pred, forecast); colnames(t_pred) <- rawNames
# dt_pred <- rbind(dt, t_pred)
# dt_pred <- ts(dt_pred, start = c(1994,3), freq=12)
# write.zoo(dt_pred, sep=',', file='../Auto_sector_pred.csv',row.names=FALSE )
