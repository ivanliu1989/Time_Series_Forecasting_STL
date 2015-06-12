setwd('/Users/ivanliu/Google Drive/ANZ/Multivariate Time Series/Auto_v1.4/R')
rm(list=ls());gc()
require(data.table);require(forecast);require(caret);require(bit64);library(vars)
dt <- fread('../data/Auto_Sector_Growth.csv', data.table=F, na.strings = '')

### Convert Data ###
timeLine <- dt[,'Date']
dt[,1] <- NULL;dt <- dt[,c(1:18,20:29,19)]
rawNames <- colnames(dt);newNames <- c(paste0('Var_', 1:(ncol(dt)-1)), 'Target')
nullRate <- apply(dt,2, function(x) mean(is.na(x)))
featMapping <- cbind(rawNames, newNames, nullRate)
colnames(dt) <- newNames

### Pre-processing ###
freq <- 12
dt <- ts(dt, freq=freq, start = c(1994,4))
for(i in 1:ncol(dt)){
    dt[which(dt[,i]==0),i] <- NA
    dt[,i] <- na.interp(dt[,i], lambda = NULL)
}
finFeatList <- featMapping[which(featMapping[,2] %in% colnames(dt)),][,1]

### VAR Modeling ###
# dt <- diff(dt, 1)
period = 24
irf_mt <- matrix(nrow = 812, ncol = 6)
z <- 1
for(j in 1:29){
    for(i in j:29){
        if(i!=j){
            fit_select <- VARselect(dt[,c(i,j)], lag.max=12, type='both');fit_select$selection
            fit <- VAR(dt[,c(i,j)], p=floor(mean(fit_select$selection)), type='both', season=NULL, ic=c("AIC", "HQ", "SC", "FPE"))
            
            cause <- causality(fit, cause = colnames(dt[,c(i,j)])[1]) #Var_1 do not Granger-cause Target
            irf_mt[z,1] <- colnames(dt[,c(i,j)])[2]; irf_mt[z,2] <- colnames(dt[,c(i,j)])[1]
            irf_mt[z,3] <- cause$Granger$p.value; irf_mt[z,4] <- cause$Instant$p.value
            cause <- causality(fit, cause = colnames(dt[,c(i,j)])[2]) #Target do not Granger-cause Var_1
            irf_mt[z,5] <- cause$Granger$p.value; irf_mt[z,6] <- cause$Instant$p.value
            if(any(irf_mt[z,3:6]<=0.05)){
                irf_fit <- irf(fit, n.ahead=24, ortho=F, cumulative=F, boot=T, ci=.95, runs=100, seed=8)
                jpeg(paste0('../Image/Impulse_Response/',gsub("[^[:alnum:]]", "", finFeatList[j]),'_to_',gsub("[^[:alnum:]]", "", finFeatList[i]),'.jpg'),width=600, height=400)
                par(mfcol=c(2,2))
                plot(irf_fit, plot.type='single')
                dev.off()
            }
            z <- z+1
        }
    }
}

colnames(irf_mt) <- c('Var1', 'Var2', 'Granger_V2', 'Instant_V2', 'Granger_V1', 'Instant_V1')
irf_mt <- irf_mt[1:406,]
write.csv(irf_mt, '../IRF_Outcomes.csv', row.names=F)
