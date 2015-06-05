setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/R/')
require(data.table)
dt <- fread('../NZ Dairy production.csv', data.table=F)
dt <- dt[,-which(names(dt) %in% c('Country', 'TARGET', 'Year','Ave precipitation'))]

# install.packages('vars')
require(vars)
data(Canada)
dt2 <- ts(dt[,1], start = 1980, freq=1)
plot.ts(dt2)
plot(dt2, type='o')

### forcast ###
# install.packages('forecast')
require(forecast)
