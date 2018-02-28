#------------------Progetto_R_Gabriele_Bisogno_2017-2018_Matricola_810106-----------------------------

#-----------------------------Data_Summary--------------------------------------

#carico librerie
library(tseries)
library(zoo)
library(PerformanceAnalytics)
library(quantmod)
library(forecast)


#Carico dati apple
getSymbols("AAPL",src = "yahoo",
  from = as.Date("2007-10-01"), to = as.Date("2017-10-31"))

getSymbols("NFLX",src = "yahoo",
  from = as.Date("2007-10-01"), to = as.Date("2017-10-31"))

getSymbols("TWX",src = "yahoo",
  from = as.Date("2007-10-01"), to = as.Date("2017-10-31"))

getSymbols("RYAAY",src = "yahoo",
  from = as.Date("2007-10-01"), to = as.Date("2017-10-31"))

getSymbols("MSFT",src = "yahoo",
  from = as.Date("2007-10-01"), to = as.Date("2017-10-31"))

getSymbols("UAL",src = "yahoo",
  from = as.Date("2007-10-01"), to = as.Date("2017-10-31"))


colnames(RYAAY)[6] <- "PriceRYAAY"
colnames(MSFT)[6] <- "PriceMSFT"
colnames(AAPL)[6] <- "PriceAAPL"
colnames(NFLX)[6] <- "PriceNFLX"
colnames(TWX)[6] <- "PriceTWX"
colnames(UAL)[6] <- "PriceUAL"

aapl.xts = AAPL$PriceAAPL
msft.xts = MSFT$PriceMSFT
ual.xts = UAL$PriceUAL
twx.xts = TWX$PriceTWX
nflx.xts = NFLX$PriceNFLX
ryaay.xts = RYAAY$PriceRYAAY

allPrice.xts = cbind(aapl.xts, msft.xts, ryaay.xts, ual.xts, nflx.xts, twx.xts)
ChartSimple = ts(allPrice.xts)
plot(ChartSimple)
plot(allPrice.xts, col=c("black","blue","green","grey","red","violet"))
legend(x="topleft", legend=c("AAPL","MSFT","RYAAY","UAL","NFLX","TWX"),
  col=c("black","blue","green","grey","red","violet"), lty=1:1)

#---------------------------Descriptive_Analytics-------------------------------
#Conversione in mensile
aaplMonthly.xts = to.monthly(aapl.xts)
msftMonthly.xts = to.monthly(msft.xts)
ryaayMonthly.xts = to.monthly(ryaay.xts)
ualMonthly.xts = to.monthly(ual.xts)
twxMonthly.xts = to.monthly(twx.xts)
nflxMonthly.xts = to.monthly(nflx.xts)

colnames(aaplMonthly.xts)[4] <- "PriceAAPL"
aaplMonthly.xts = aaplMonthly.xts$PriceAAPL

colnames(msftMonthly.xts)[4] <- "PriceMSFT"
msftMonthly.xts = msftMonthly.xts$PriceMSFT

colnames(ryaayMonthly.xts)[4] <- "PriceRYAAY"
ryaayMonthly.xts = ryaayMonthly.xts$PriceRYAAY

colnames(ualMonthly.xts)[4] <- "PriceUAL"
ualMonthly.xts = ualMonthly.xts$PriceUAL

colnames(nflxMonthly.xts)[4] <- "PriceNFLX"
nflxMonthly.xts = nflxMonthly.xts$PriceNFLX

colnames(twxMonthly.xts)[4] <- "PriceTWX"
twxMonthly.xts = twxMonthly.xts$PriceTWX


#compute simple monthly returns
aaplSimpleMonthlyReturns.xts =
  CalculateReturns(aaplMonthly.xts , method="simple")
msftSimpleMonthlyReturns.xts =
  CalculateReturns(msftMonthly.xts , method="simple")
ryaaySimpleMonthlyReturns.xts =
  CalculateReturns(ryaayMonthly.xts , method="simple")
ualSimpleMonthlyReturns.xts =
  CalculateReturns(ualMonthly.xts , method="simple")
nflxSimpleMonthlyReturns.xts =
  CalculateReturns(nflxMonthly.xts , method="simple")
twxSimpleMonthlyReturns.xts =
  CalculateReturns(twxMonthly.xts , method="simple")

SMmerge = cbind(aaplSimpleMonthlyReturns.xts,msftSimpleMonthlyReturns.xts,
            ryaaySimpleMonthlyReturns.xts,ualSimpleMonthlyReturns.xts,
            nflxSimpleMonthlyReturns.xts, twxSimpleMonthlyReturns.xts)

plot(SMmerge, col=c("black","blue","green","grey","red","violet"))
legend(x="topright", legend=c("AAPL","MSFT","RYAAY","UAL","NFLX","TWX"),
  col=c("black","blue","green","grey","red","violet"), lty=1:1)

#compute compounded monthly returns
aaplCompundedMonthlyReturns.xts =
  CalculateReturns(aaplMonthly.xts , method="compound")
msftCompundedMonthlyReturns.xts =
  CalculateReturns(msftMonthly.xts , method="compound")
ryaayCompundedMonthlyReturns.xts =
  CalculateReturns(ryaayMonthly.xts , method="compound")
ualCompundedMonthlyReturns.xts =
  CalculateReturns(ualMonthly.xts , method="compound")
nflxCompundedMonthlyReturns.xts =
  CalculateReturns(nflxMonthly.xts , method="compound")
twxCompundedMonthlyReturns.xts =
  CalculateReturns(twxMonthly.xts , method="compound")

CCMmerge = cbind(aaplCompundedMonthlyReturns.xts,
            msftCompundedMonthlyReturns.xts,
            ryaayCompundedMonthlyReturns.xts,
            ualCompundedMonthlyReturns.xts,
            nflxCompundedMonthlyReturns.xts,
            twxCompundedMonthlyReturns.xts)

plot(CCMmerge, col=c("black","blue","green","grey","red","violet"))
legend(x="topright", legend=c("AAPL","MSFT","RYAAY","UAL","NFLX","TWX"),
  col=c("black","blue","green","grey","red","violet"), lty=1:1)

#Diagnostic plots

#compunded_monthly
par(mfrow=c(2,3))

aapl.mat = coredata(aaplCompundedMonthlyReturns.xts)
hist(aapl.mat,main="Histogram of AAPL monthly returns",
  probability=TRUE, col="black", breaks = 20)

msft.mat = coredata(msftCompundedMonthlyReturns.xts)
hist(msft.mat,main="Histogram of MSFT monthly returns",
  probability=TRUE, col="blue", breaks = 20)

ual.mat = coredata(ualCompundedMonthlyReturns.xts)
hist(ual.mat,main="Histogram of UAL monthly returns",
  probability=TRUE, col="grey", breaks = 20)

ryaay.mat = coredata(ryaayCompundedMonthlyReturns.xts)
hist(ryaay.mat,main="Histogram of RYAAY monthly returns",
  probability=TRUE, col="green", breaks = 20)

nflx.mat = coredata(nflxCompundedMonthlyReturns.xts)
hist(nflx.mat,main="Histogram of NFLX monthly returns",
  probability=TRUE, col="red", breaks = 20)

twx.mat = coredata(twxCompundedMonthlyReturns.xts)
hist(twx.mat,main="Histogram of TWX monthly returns",
  probability=TRUE, col="violet", breaks = 20)



#Smoothed density plots

#compunded_monthly
par(mfrow=c(2,3))

aapl.density = density(aapl.mat, na.rm=TRUE)
plot(aapl.density,type="l",xlab="returns", col="black", lwd=2,
  ylab="density estimate",main="Smoothed AAPL histogram")

msft.density = density(msft.mat, na.rm=TRUE)
plot(msft.density,type="l",xlab="returns", col="blue", lwd=2,
  ylab="density estimate",main="Smoothed MSFT histogram")

ual.density = density(ual.mat, na.rm=TRUE)
plot(ual.density,type="l",xlab="returns", col="grey", lwd=2,
  ylab="density estimate",main="Smoothed UAL histogram")

ryaay.density = density(ryaay.mat, na.rm=TRUE)
plot(ryaay.density,type="l",xlab="returns", col="green", lwd=2,
  ylab="density estimate",main="Smoothed RYAAY histogram")

nflx.density = density(nflx.mat, na.rm=TRUE)
plot(nflx.density,type="l",xlab="returns", col="red", lwd=2,
  ylab="density estimate",main="Smoothed NFLX histogram")

twx.density = density(twx.mat, na.rm=TRUE)
plot(twx.density,type="l",xlab="returns", col="violet", lwd=2,
  ylab="density estimate",main="Smoothed TWX histogram")

#Boxplot plots

#compunded_monthly
par(mfrow=c(2,3))

boxplot(aapl.mat, main="AAPL boxplot",
  ylab="monthly AAPL return", col="black")


boxplot(msft.mat, main="MSFT boxplot",
  ylab="monthly MSFT return", col="blue")


boxplot(ual.mat, main="UAL boxplot",
  ylab="monthly ual return", col="grey")


boxplot(ryaay.mat, main="RYAAY boxplot",
  ylab="monthly RYAAY return", col="green")


boxplot(nflx.mat, main="NFLX boxplot",
  ylab="monthly NFLX return", col="red")


boxplot(twx.mat, main="TWX boxplot",
  ylab="monthly TWX return", col="violet")


#QQ Plots

#compunded_monthly
par(mfrow=c(2,3))

qqnorm(aapl.mat, main="AAPL", col="black")

qqnorm(msft.mat, main="MSFT", col="blue")

qqnorm(ual.mat, main="UAL", col="grey")

qqnorm(ryaay.mat, main="RYAAY", col="green")

qqnorm(nflx.mat, main="NFLX", col="red")

qqnorm(twx.mat, main="TWX", col="violet")


#boxplot over data merged
par(mfrow=c(1,1))
boxplot(cbind(aapl.mat,msft.mat,ual.mat,ryaay.mat,nflx.mat,twx.mat))


#Univariate Descriptive Statistics
#mean
meanMerge = cbind(mean(aapl.mat, na.rm = TRUE),
                  mean(msft.mat, na.rm = TRUE),
                  mean(ual.mat, na.rm = TRUE),
                  mean(ryaay.mat, na.rm = TRUE),
                  mean(nflx.mat, na.rm = TRUE),
                  mean(twx.mat, na.rm = TRUE))

#rinominare prima riga e poi colonne, e non avviare queste righe di codice insieme 
rownames(meanMerge)[1] <- "values"
colnames(meanMerge)[1] <- "meanAAPL"
colnames(meanMerge)[2] <- "meanMSFT"
colnames(meanMerge)[3] <- "meanUAL"
colnames(meanMerge)[4] <- "meanRYAAY"
colnames(meanMerge)[5] <- "meanNFLX"
colnames(meanMerge)[6] <- "meanTWX"


#var
varMerge = cbind(var(aapl.mat, na.rm = TRUE),
                  var(msft.mat, na.rm = TRUE),
                  var(ual.mat, na.rm = TRUE),
                  var(ryaay.mat, na.rm = TRUE),
                  var(nflx.mat, na.rm = TRUE),
                  var(twx.mat, na.rm = TRUE))

rownames(varMerge)[1] <- "values"
colnames(varMerge)[1] <- "varAAPL"
colnames(varMerge)[2] <- "varMSFT"
colnames(varMerge)[3] <- "varUAL"
colnames(varMerge)[4] <- "varRYAAY"
colnames(varMerge)[5] <- "varNFLX"
colnames(varMerge)[6] <- "varTWX"

#sd
sdMerge = cbind(sd(aapl.mat, na.rm = TRUE),
                  sd(msft.mat, na.rm = TRUE),
                  sd(ual.mat, na.rm = TRUE),
                  sd(ryaay.mat, na.rm = TRUE),
                  sd(nflx.mat, na.rm = TRUE),
                  sd(twx.mat, na.rm = TRUE))

rownames(sdMerge)[1] <- "values"
colnames(sdMerge)[1] <- "sdAAPL"
colnames(sdMerge)[2] <- "sdMSFT"
colnames(sdMerge)[3] <- "sdUAL"
colnames(sdMerge)[4] <- "sdRYAAY"
colnames(sdMerge)[5] <- "sdNFLX"
colnames(sdMerge)[6] <- "sdTWX"

#skewness
skewnessMerge = cbind(skewness(aapl.mat, na.rm = TRUE),
                  skewness(msft.mat, na.rm = TRUE),
                  skewness(ual.mat, na.rm = TRUE),
                  skewness(ryaay.mat, na.rm = TRUE),
                  skewness(nflx.mat, na.rm = TRUE),
                  skewness(twx.mat, na.rm = TRUE))

rownames(skewnessMerge)[1] <- "values"
colnames(skewnessMerge)[1] <- "skewnessAAPL"
colnames(skewnessMerge)[2] <- "skewnessMSFT"
colnames(skewnessMerge)[3] <- "skewnessUAL"
colnames(skewnessMerge)[4] <- "skewnessRYAAY"
colnames(skewnessMerge)[5] <- "skewnessNFLX"
colnames(skewnessMerge)[6] <- "skewnessTWX"


#kurtosis
kurtosisMerge = cbind(kurtosis(aapl.mat, na.rm = TRUE),
                  kurtosis(msft.mat, na.rm = TRUE),
                  kurtosis(ual.mat, na.rm = TRUE),
                  kurtosis(ryaay.mat, na.rm = TRUE),
                  kurtosis(nflx.mat, na.rm = TRUE),
                  kurtosis(twx.mat, na.rm = TRUE))

rownames(kurtosisMerge)[1] <- "values"
colnames(kurtosisMerge)[1] <- "kurtosisAAPL"
colnames(kurtosisMerge)[2] <- "kurtosisMSFT"
colnames(kurtosisMerge)[3] <- "kurtosisUAL"
colnames(kurtosisMerge)[4] <- "kurtosisRYAAY"
colnames(kurtosisMerge)[5] <- "kurtosisNFLX"
colnames(kurtosisMerge)[6] <- "kurtosisTWX"


#quantile
quantileMerge = cbind(quantile(aapl.mat, na.rm = TRUE),
                  quantile(msft.mat, na.rm = TRUE),
                  quantile(ual.mat, na.rm = TRUE),
                  quantile(ryaay.mat, na.rm = TRUE),
                  quantile(nflx.mat, na.rm = TRUE),
                  quantile(twx.mat, na.rm = TRUE))

colnames(quantileMerge)[1] <- "quantileAAPL"
colnames(quantileMerge)[2] <- "quantileMSFT"
colnames(quantileMerge)[3] <- "quantileUAL"
colnames(quantileMerge)[4] <- "quantileRYAAY"
colnames(quantileMerge)[5] <- "quantileNFLX"
colnames(quantileMerge)[6] <- "quantileTWX"


#Sample covariance matrix
SCovMatr = var(cbind(aapl.mat, msft.mat, ual.mat, ryaay.mat, nflx.mat, twx.mat),
  na.rm = TRUE)
#Sample correlation matrix
SCorMatr = cor(cbind(aapl.mat, msft.mat, ual.mat, ryaay.mat, nflx.mat, twx.mat),
  use = "pairwise.complete.obs")

#plot pairwise scatterplot
pairs(cbind(aapl.mat, msft.mat, ual.mat, ryaay.mat, nflx.mat, twx.mat),
  col="blue", pch=18, cex=1.5, cex.axis=1.5)

#scatterplot of most correlated istruments
plot(aapl.mat,msft.mat,
  main="Monthly cc returns on AAPL and MSFT",  pch=18,  col=c("black", "blue"))


#-------------------------Predictive analytics----------------------------------


aapl.z= as.zoo(aaplMonthly.xts)
msft.z= as.zoo(msftMonthly.xts)
ryaay.z= as.zoo(ryaayMonthly.xts)
ual.z= as.zoo(ualMonthly.xts)
nflx.z= as.zoo(nflxMonthly.xts)
twx.z= as.zoo(twxMonthly.xts)

#APPL forecasting
fit <- stl( aapl.z[,1], s.window="period" )
returns <- diff( log(aapl.z[,1]) )
fitRet <- stl( returns[,1], s.window="period" )

returnsTrain <- returns[1:80]  # Train dataset
returnsTest <- returns[80: 110]  # Test dataset
returnsL <- returns[110 : 120]
fit <- arima(returnsTrain, order = c(3, 0, 3))

arma.preds <- predict(fit, n.ahead = 30)$pred
arma.forecast <- forecast(fit, h = 40)
plot(arma.forecast, main = "ARMA forecasts for AAPL returns")

accuracy(arma.preds, returnsL)[2] # RMSE values

lines( returnsTest )


#MSFT forecasting
t <- stl( msft.z[,1], s.window="period" )
returns <- diff( log(msft.z[,1]) )
fitRet <- stl( returns[,1], s.window="period" )

returnsTrain <- returns[1:80]  # Train dataset
returnsTest <- returns[80: 110]  # Test dataset
returnsL <- returns[110 : 120]
fit <- arima(returnsTrain, order = c(3, 0, 3))

arma.preds <- predict(fit, n.ahead = 30)$pred
arma.forecast <- forecast(fit, h = 40)
plot(arma.forecast, main = "ARMA forecasts for MSFT returns")

accuracy(arma.preds, returnsL)[2] # RMSE values

lines( returnsTest )


#RYAAY forecasting
t <- stl( ryaay.z[,1], s.window="period" )
returns <- diff( log(ryaay.z[,1]) )
fitRet <- stl( returns[,1], s.window="period" )

returnsTrain <- returns[1:80]  # Train dataset
returnsTest <- returns[80: 110]  # Test dataset
returnsL <- returns[110 : 120]
fit <- arima(returnsTrain, order = c(3, 0, 3))

arma.preds <- predict(fit, n.ahead = 30)$pred
arma.forecast <- forecast(fit, h = 40)
plot(arma.forecast, main = "ARMA forecasts for RYAAY returns")

accuracy(arma.preds, returnsL)[2] # RMSE values

lines( returnsTest )



#ual forecasting
t <- stl( ual.z[,1], s.window="period")
returns <- diff( log(ual.z[,1]) )
fitRet <- stl( returns[,1], s.window="period" )

returnsTrain <- returns[1:80]  # Train dataset
returnsTest <- returns[80: 110]  # Test dataset
returnsL <- returns[110 : 120]
fit <- arima(returnsTrain, order = c(3, 0, 3))

arma.preds <- predict(fit, n.ahead = 30)$pred
arma.forecast <- forecast(fit, h = 40)
plot(arma.forecast, main = "ARMA forecasts for UAL returns")

accuracy(arma.preds, returnsL)[2] # RMSE values

lines( returnsTest )



#NFLX forecasting
t <- stl( nflx.z[,1], s.window="period" )
returns <- diff( log(nflx.z[,1]) )
fitRet <- stl( returns[,1], s.window="period" )

returnsTrain <- returns[1:80]  # Train dataset
returnsTest <- returns[80: 110]  # Test dataset
returnsL <- returns[110 : 120]
fit <- arima(returnsTrain, order = c(3, 0, 3))

arma.preds <- predict(fit, n.ahead = 30)$pred
arma.forecast <- forecast(fit, h = 40)
plot(arma.forecast, main = "ARMA forecasts for NFLX returns")

accuracy(arma.preds, returnsL)[2] # RMSE values

lines( returnsTest )



#TWX forecasting
t <- stl( twx.z[,1], s.window="period" )
returns <- diff( log(twx.z[,1]) )
fitRet <- stl( returns[,1], s.window="period" )

returnsTrain <- returns[1:80]  # Train dataset
returnsTest <- returns[80: 110]  # Test dataset
returnsL <- returns[110 : 120]
fit <- arima(returnsTrain, order = c(3, 0, 3))

arma.preds <- predict(fit, n.ahead = 30)$pred
arma.forecast <- forecast(fit, h = 40)
plot(arma.forecast, main = "ARMA forecasts for TWX returns")

accuracy(arma.preds, returnsL)[2] # RMSE values

lines( returnsTest )




#---------------------------------END-------------------------------------------
