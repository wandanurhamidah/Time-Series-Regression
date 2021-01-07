#ARIMA model for Google Trends daTa

library(lmtest)
library(tseries)
library(forecast)

data1 <- read.csv("C:/Users/Administrator/Documents/DSC/Pertemuan 10/datatugas6.csv", sep=";", header=T)
head(data1)
View(data1)
ylt <- as.ts(data1[,2])
ytrain = as.ts(ylt[1:144])
ytest = as.ts(ylt[145:168])

#TIME SERIES PLOT#
par(mfrow=c(1,1),mar=c(3.1,3.2,1,0.3),mgp=c(2.1,0.5,0))
plot(ytrain,axes=F,ylab="yt", xlab="Hour")
title("Gojek Indonesia",line=0.3,cex.main=0.9)
box()
axis(side=2,lwd=0.5,cex.axis=0.7,las=2,cex=0.5)
axis(side=1,at=seq(1,144,24),lwd=0.5,cex.axis=0.8,las=0)
points(ytrain,col="red3",cex=0.75,pch=19)

#BoxCox
BoxCox.lambda(ylt,method = "guerrero",lowe=-1,upper=2)
ytrain = as.ts(transy[1:144])                             #define training data
ytest = as.ts(transy[145:168])                            #define testing data

#CHECKING FOR STATIONARY USING ACF PLOT#
tick=c(1,24,48,72)
par(mfrow=c(2,1),mar=c(2.8,3,1.2,0.4))
par(mgp=c(1.7,0.5,0))
#ACF
acf(ytrain,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)
#PACF
pacf(ytrain,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)

#CHECKING FOR STATIONARY USING ADF (Augmented Dickey Fuller) TEST#
adf.test(ytrain, k=24)

#DIFFERENCING SEASONAL ORDER FOR YTRAIN
wtrain=diff(ytrain,lag=24)
par(mfrow=c(1,1))
plot(wtrain)

#CHECKING FOR STATIONARY USING ADF TEST#
adf.test(wtrain, k=10)

#ACF
acf(wtrain,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)
#PACF
pacf(wtrain,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)

#DIFFERENCING NON-SEASONAL ORDER FOR WTRAIN
Ztrain=diff(wtrain,lag=1)
par(mfrow=c(1,1))
plot(Ztrain)
adf.test(Ztrain)

#ORDER IDENTIFICATION USING ACF AND PACF FROM STATIONARY DATA
tick=c(1,24,48,72)
par(mfrow=c(2,1),mar=c(2.8,3,1.2,0.4))
par(mgp=c(1.7,0.5,0))
#ACF
acf(Ztrain,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)
#PACF
pacf(Ztrain,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)

#1st ARIMA model
#seasonal ARIMA model (meet white noise assumption)
modelARIMA=arima(ytrain, order = c(0,1,1),
                 seasonal = list(order = c(0,1,1),
                                 period =24),
                 include.mean=TRUE, method = c("ML"))
summary(modelARIMA)                                        
coeftest(modelARIMA)                                       #significance test for parameter
resi.ARIMA=as.ts(modelARIMA$residuals)                     #define the residual value
fits.ARIMA=as.ts(fitted(modelARIMA))                       #define forecast value for training data

#2nd ARIMA model
#seasonal ARIMA model (meet white noise assumption)
modelARIMA=arima(ytrain, order = c(0,1,25),
                 seasonal = list(order = c(0,1,1),period =24),
                 transform.pars = FALSE, 
                 fixed=c(NA,rep(0,21), NA, rep(0,1), NA, NA),  #NA was the estimated lag
                 include.mean=TRUE, method = c("ML"))
summary(modelARIMA)                                        
coeftest(modelARIMA)                                       #significance test for parameter
resi.ARIMA=as.ts(modelARIMA$residuals)                     #define the residual value
fits.ARIMA=as.ts(fitted(modelARIMA))                       #define forecast value for training data

#PLOT
par(mfrow=c(1,1))
plot(ytrain)
lines(fits.ARIMA, col="red")

#DIAGNOSTIC CHECKING FOR ARIMA MODEL
#Independency test by using Ljung-Box test
lags <- c(6,12,18,24,30,36,42,48)
p=0
q=1
LB.result <- matrix(0,length(lags),2)
for(i in seq_along(lags))
{
  LB.test=Box.test(resi.ARIMA, lag = lags[i],type = c("Ljung-Box"),fitdf=p+q)
  LB.result[i,1]=LB.test$statistic
  LB.result[i,2]=LB.test$p.value
}
rownames(LB.result)<-lags
colnames(LB.result)<-c("statistics","p.value")
LB.result

#ACF and PACF for RESIDUAL ARIMA MODEL
tick=c(1,24,48,72)
par(mfrow=c(2,1),mar=c(2.8,3,1.2,0.4))
par(mgp=c(1.7,0.5,0))
#ACF
acf(resi.ARIMA,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)
#PACF
pacf(resi.ARIMA,lag.max=72,axes=F,ylim=c(-1,1))
box()
axis(side=1,at=tick,label=tick,lwd=0.5,las=0,cex.axis=0.8)
axis(side=2,lwd=0.5,las=2,cex=0.5,cex.axis=0.8)

#COMPARISON BETWEEN ACTUAL AND FORECAST VALUE
a=min(min(fits.ARIMA),min(ytrain))                       #lower bound for training data
b=max(max(fits.ARIMA),max(ytrain))                       #upper bound for training data

par(mfrow=c(1,2),mar=c(2.3,2.7,1.2,0.4))     #the number of picture and its margin
par(mgp=c(1.3,0.5,0))                        #the distance between labels and axis

#PLOTTING FOR TRAINING DATA#
plot(as.ts(ytrain),ylab="yt",xlab="t",lwd=2,axes=F,ylim=c(a*0.9,b*1.1))
box()
title("Training",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=0)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=seq(1,144,24))
lines(as.ts(fits.ARIMA),col="red",lwd=2)

#PLOTTING FOR TESTING DATA#
plot(as.ts(ytest),ylab="yt",xlab="t",lwd=2,ylim=c(a*0.9,b*1.1),cex.lab=0.8,axes=F)
box()
title("Testing",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=0)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:24),labels=c(145:168))

#DEFINE THE LEGEND#
legend("topright",c("Actual","Forecast","Upper Bound","Lower Bound"),
       col=c("black","red","blue2","blue2"),lwd=2,cex=0.6)

#CALCULATE RMSE, MAE, AND MAPE CRITERIA
accuracies=matrix(0,3,2)
colnames(accuracies)=c("Training","Testing")
rownames(accuracies)=c("RMSE","MAE","MAPE")

accuracies[1,1]=accuracy(fits.ARIMA,ytrain)[1,2]
accuracies[2,1]=accuracy(fits.ARIMA,ytrain)[1,3]
accuracies[3,1]=accuracy(fits.ARIMA,ytrain)[1,5]
accuracies
