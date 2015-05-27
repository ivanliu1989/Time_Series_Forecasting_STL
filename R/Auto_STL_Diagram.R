setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/Time_Series_Forecasting_STL/R/')
require(data.table);require(forecast)
dt <- fread('../data/Auto_Sector_comp.csv', data.table=F)
dt <- dt[,-which(names(dt) %in% c('Date'))]
dt <- ts(dt, start = c(1994,3), freq=12)

for(i in 1:ncol(dt)){
    fit <- stl(dt[,i], s.window="period")
    jpeg(paste0('../image/',gsub("[^[:alnum:]]", "", colnames(dt)[i]),'_STL.jpg'))
    plot(fit,main = colnames(dt)[i])
    dev.off()
}


fit <- stl(dt[,29], s.window="periodic")
plot(fit)
plot(forecast(fit))
plot(stlf(dt[,29], lambda=BoxCox.lambda(dt[,29])))
