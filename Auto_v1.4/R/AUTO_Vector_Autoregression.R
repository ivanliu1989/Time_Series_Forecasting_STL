setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/Auto_v1.4/R')
rm(list=ls());gc()
require(data.table);require(forecast);require(caret);require(bit64);library(vars)
dt <- fread('../data/Auto_Sector_Growth.csv', data.table=F, na.strings = '')
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
# for(i in 1:ncol(dt)){
#     dn<-ndiffs(dt[,i])
#     #dt[,i] <- c(rep("",dn),diff(dt[,i], differences = dn))
#     print(paste0('Var', i, ': ',dn))
# }
# dt <- diff(dt, differences = 1)
# dt <- dt[,-c(7,8)]
# finFeatList <- finFeatList[-c(7,8)]
dt_t <- ts(dt[train,],freq=freq,start=c(1994,5))
# fit_select <- VARselect(dt_t, lag.max=12, type='both')
# fit_select$selection
fit <- VAR(dt, p=4, type='both', season=NULL, ic=c("AIC", "HQ", "SC", "FPE"))
irf_fit <- irf(fit, n.ahead=24, ortho=F, cumulative=F, boot=T, ci=.95, runs=100, seed=8)

# plot(fit)
result <- forecast(fit, h=period)
for(i in 1:ncol(dt)){
    jpeg(paste0('../Image/VAR_Validation/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_validation.jpg'),width=600, height=400)
    plot(c(dt[,i]),type = 'l', xaxt='n',main = paste0(finFeatList[i]),
         ylim=c(min(dt[,i],result$lower[[i]][,2]),max(dt[,i],result$upper[[i]][,2])), ylab=finFeatList[i]);
    lines(c(rep(NA,length(dt_t[,i])),result$lower[[i]][,2]), col='blue');
    lines(c(rep(NA,length(dt_t[,i])),result$lower[[i]][,1]), col='orange');
    lines(c(rep(NA,length(dt_t[,i])),result$upper[[i]][,2]), col='blue');
    lines(c(rep(NA,length(dt_t[,i])),result$upper[[i]][,1]), col='orange')
    lines(c(rep(NA,length(dt_t[,i])),result$mean[[i]]), col='red')
    axis(side=1,at=1:length(timeLine),label=timeLine)
    dev.off()
}

dt_t <- ts(dt,freq=freq,start=c(1994,5))
fit <- VAR(dt_t, p=4, type='both', season=NULL, ic=c("AIC", "HQ", "SC", "FPE"))
# plot(fit)
result <- forecast(fit, h=period)
for(i in 1:ncol(dt)){
    jpeg(paste0('../Image/VAR/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_validation.jpg'),width=600, height=400)
    plot(c(dt[,i],rep(NA,length(result$lower[[i]][,2]))),type = 'l', xaxt='n', 
         ylim=c(min(dt[,i],result$lower[[i]][,2]),max(dt[,i],result$upper[[i]][,2])), ylab=finFeatList[i]);
    lines(c(dt[,i],result$lower[[i]][,1]), col='orange');lines(c(dt[,i],result$upper[[i]][,1]), col='orange');
    lines(c(dt[,i],result$lower[[i]][,2]), col='blue');lines(c(dt[,i],result$upper[[i]][,2]), col='blue')
    lines(c(rep(NA,length(dt[,i])),result$mean[[i]]), col='red')
    axis(side=1,at=1:length(timeLine),label=timeLine)
    dev.off()
}
# 
# result <- predict(fit, n.ahead = period, ci=0.95, dumvar = NULL)
# for(i in 1:ncol(dt)){
#     jpeg(paste0('../Image/VAR_Validation_2/',gsub("[^[:alnum:]]", "", finFeatList[i]),'_validation.jpg'),width=600, height=400)
#     plot(c(dt[,i]),type = 'l', xaxt='n',main = paste0(finFeatList[i]),
#          ylim=c(min(dt[,i],result$fcst[[i]][,3]),max(dt[,i],result$fcst[[i]][,2])), ylab=finFeatList[i]);
#     lines(c(rep(NA,length(dt_t[,i])),result$fcst[[i]][,2]), col='blue');
#     lines(c(rep(NA,length(dt_t[,i])),result$fcst[[i]][,4]), col='orange');
#     lines(c(rep(NA,length(dt_t[,i])),result$fcst[[i]][,3]), col='blue');
#     lines(c(rep(NA,length(dt_t[,i])),result$fcst[[i]][,1]), col='red')
#     axis(side=1,at=1:length(timeLine),label=timeLine)
#     dev.off()
# }

#####################
### SVAR Modeling ###
#####################
# amat <- diag(29)
# diag(amat) <- NA
# amat[2, 1] <- NA
# amat[4, 1] <- NA
# fit2 <- SVAR(fit, estmethod = "scoring", max.iter=1000, Amat = amat, Bmat = NULL,maxls = 1000, conv.crit = 1.0e-8)
# p <- predict(fit, dt)
# plot(p$fcst$Target[,1])

