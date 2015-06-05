setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/NZ_Dairy_v1.2/R')
rm(list=ls());gc();source('pairs.r')
require(data.table);require(forecast);require(caret);require(bit64)
weather <- fread('../Data/NZ_Dairy_Weather.csv', data.table=F)
dt <- fread('../data/WorldBankData-NZ-AU-v1.csv', data.table=F, na.strings = '')

weather$year <- substr(weather[,1],1,4)
maxTemp <- aggregate(as.numeric(weather[,3]), list(weather$year), FUN=function(x) mean(x, na.rm = T))
minTemp <- aggregate(as.numeric(weather[,4]), list(weather$year), FUN=function(x) mean(x, na.rm = T))
rainFall <- aggregate(as.numeric(weather[,5]), list(weather$year), FUN=function(x) mean(x, na.rm = T))
colnames(maxTemp) <- c('Indicators', 'maxTemp')
colnames(minTemp) <- c('Indicators', 'minTemp')
colnames(rainFall) <- c('Indicators', 'rainFall')
dt <- merge(dt, maxTemp, all.x=T)
dt <- merge(dt, minTemp, all.x=T)
dt <- merge(dt, rainFall, all.x=T)


write.csv(dt, '../data/WorldBankData-NZ-AU-v1.1.csv', quote = T, row.names = F)
