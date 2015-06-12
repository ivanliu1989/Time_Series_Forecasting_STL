setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/Auto_v1.4/R')
rm(list=ls());gc()
require(data.table);require(forecast);require(caret);require(bit64)
dt <- fread('../data/Auto_Sector_Growth.csv', data.table=F, na.strings = '')
dt2 <- fread('../data/Auto_Sector.csv', data.table=F, na.strings = '')
dt <- dt2
####################
### Convert Data ###
####################
timeLine <- dt[,'Date']
dt[,1] <- NULL; dt2[,1] <- NULL# 1994/04/01 
dt <- dt[,c(1:18,20:29,19)];dt2 <- dt2[,c(1:18,20:29,19)]
rawNames <- colnames(dt);newNames <- c(paste0('Var_', 1:(ncol(dt)-1)), 'Target')
nullRate <- apply(dt,2, function(x) mean(is.na(x)))
featMapping <- cbind(rawNames, newNames, nullRate)
colnames(dt) <- newNames

######################
### Pre-processing ###
######################
freq <- 12
dt <- ts(dt, freq=freq, start = c(1994,4));dt2 <- ts(dt2, freq=freq, start = c(1994,4))
for(i in 1:ncol(dt)){
    dt[which(dt[,i]==0),i] <- NA
    dt[,i] <- na.interp(dt[,i], lambda = NULL)
}
for(i in 1:ncol(dt2)){
    dt2[which(dt2[,i]==0),i] <- NA
    dt2[,i] <- na.interp(dt2[,i], lambda = NULL)
}

###################
### Correlation ###
###################
finFeatList <- featMapping[which(featMapping[,2] %in% colnames(dt)),][,1]
lags <- 5*12
for(i in 1:ncol(dt)){
    for(j in 1:ncol(dt)){
        jpeg(paste0('../Image/Lagged_Predictors_Other/Var',i,'/',gsub("[^[:alnum:]]", "", finFeatList[j]),'.jpg'),width=600, height=400)
        x <- dt[,j]; y <- dt[,i]
        # x <- (log(x) - mean(log(x))); y <- (log(y) - mean(log(y)))
        diff1x <- diff(x,1);diff1y <- diff(y,1)
        # diff1x = fracdiff((log(x) - mean(log(x))), nar=0, nma=0,M=30)
        # diff1y = fracdiff((log(y) - mean(log(y))), nar=0, nma=0,M=30)
        # diff1x = diffseries((log(x) - mean(log(x))), diff1x$d)
        # diff1y = diffseries((log(y) - mean(log(y))), diff1y$d)
        cffvalues <- ccf(diff1x,diff1y,na.action=na.omit,lag.max=lags, plot=T,ylab = "cross-correlation",ylim=c(-1,1),
                         main=paste0(finFeatList[j]," & ",finFeatList[i]))
        dev.off()    
    }
}

####################
### PCA analysis ###
####################
pca <- preProcess(dt[,-29],method=c("center", "scale", "pca"),pcaComp = 1)
dt_pca <- predict(pca, dt[,-29])
#prcomp
diff1x <- diff(ts(dt_pca[,1], freq=12, start = c(1994,4)), 1)
diff1y <- diff(dt[,29], 1)
cffvalues <- ccf(diff1x,diff1y,na.action=na.omit,lag.max=lags, plot=T,ylab = "cross-correlation",#ylim=c(-1,1),
                 main="PCA1 & Auto Sales Volume")
