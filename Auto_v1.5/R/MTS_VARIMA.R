setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/Auto_v1.4/R')
rm(list=ls());gc()
require(data.table);require(forecast);require(caret);require(bit64)
dt <- fread('../data/Auto_Sector.csv', data.table=F, na.strings = '')
####################
### Convert Data ###
####################
timeLine <- dt[,'Date']
dt[,1] <- NULL;# 1994/04/01 
dt <- dt[,c(1:18,20:29,19)]
rawNames <- colnames(dt);newNames <- c(paste0('Var_', 1:(ncol(dt)-1)), 'Target')
nullRate <- apply(dt,2, function(x) mean(is.na(x)))
featMapping <- cbind(rawNames, newNames, nullRate)
colnames(dt) <- newNames

######################
### Pre-processing ###
######################
freq <- 12
dt <- ts(dt, freq=freq, start = c(1994,4))
for(i in 1:ncol(dt)){
    dt[which(dt[,i]==0),i] <- NA
    dt[,i] <- na.interp(dt[,i], lambda = NULL)
}

####################
### VAR Modeling ###
####################
### parameter ###
finFeatList <- featMapping[which(featMapping[,2] %in% colnames(dt)),][,1]
period = 24
train = 1:(nrow(dt)-period)
test = (nrow(dt)-period+1):nrow(dt)

### Model ###
require(MTS)

varma_fit <- VARMACpp(dt[,1:10], p = 6)
var_fit <- VAR(dt, p = 1, output = T, include.mean = T, fixed = NULL)
result <- VARpred(var_fit, h=12)
