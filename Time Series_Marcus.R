setwd("C:/Users/marcu/Downloads")

data <- read.csv2("~/Dataset FINAL(paperstyle).csv", stringsAsFactors=FALSE)

install.packages("lmtest")
install.packages("sandwich")
install.packages("dyn")
install.packages("forecast")
install.packages("tseries")
install.packages("aod")

library(lmtest)
library(sandwich)
library(dyn) #this package includes extensions to time series applications for basic commands
library(forecast)
library(tseries)
library(aod)
library(foreign)
library(plm) 
library(car)

data$Date<-as.Date(as.character(data$Date), "%d.%m.%Y")
#data <- pdata.frame(data, index = c("Company", "Date"), drop.index = F, row.names = T)

newdata2 <- subset(data, data$Company==1) 

#Since we are working with Time Series now, convert the data to a time series object
newdata2 <- ts(newdata2,start=c(1996,3), end=c(2015,4),frequency=4)

plot(newdata2[,"Stock"], type="l", lwd=2, col="red", xlab="Date", ylab="Stock")
newdata2

lin.model <- dyn$lm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=newdata2)
summary(lin.model)                   

#joint significance of ind. variables
vcov(lin.model)
wald.test(b=coef(lin.model), Sigma=vcov(lin.model),Terms=1:7)

res.a <- as.ts(lin.model$residuals) #extract the residuals from the model, and convert them into a time series
#looking at res.
plot(res.a)
lin.model2 <- dyn$lm(Stock ~ Market, data=newdata2)
summary(lin.model2)
Stock<-newdata2[,"Stock"]
Market<-newdata2[,"Market"]
ts.plot(Stock,type="p")
#have a look at autocorrelation in the residuals, using the acf() function
acf(res.a,50)
acf(res.a,50,plot=F)
#If the residuals WERE serially correlated, the Gauss-Markov Assumptions do not hold and ols is not BLUE

#and at partial autocorrelations
pacf(res.a,50)

#regress the residuals of on their lags of -1 to -4 (4 quarters) (so we try to explain residuals by time-shifted residuals, e.g. resid. in t=2 by resid. in t=1)
resa.lags <-dyn$lm(res.a~lag(res.a,-1)+lag(res.a,-2)+lag(res.a,-3)+lag(res.a,-4))
summary(resa.lags)
#test for joint significance of the montly lagged residuals (Breusch-Godfrey Test)
vcov(resa.lags)
wald.test(b=coef(resa.lags), Sigma=vcov(resa.lags), Terms=2:4)
#highly insignificant, which indicates no serial correlation

#If we HAD detected serial corr.--> Respecified Model to make it dynamically complete, by including lags of the dependent variable:
#Dynamically complete models should not have any serial correlation in the error term

#2 lags:
dyn.complete2 <- dyn$lm(Stock~lag(Stock,-1)+lag(Stock,-2)+Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=newdata2)
summary(dyn.complete2)
#lags not significant => model w/o lags already dynamically complete

#If we wanted to get a complete model through lags --> Again we have to test: Serial correlation in residuals?
res.2 <- as.ts(dyn.complete2$residuals) 
res2.lags <-dyn$lm(res.2~lag(res.2,-1)+lag(res.2,-2)+lag(res.2,-3)+lag(res.2,-4))
summary(res2.lags)

wald.test(b=coef(res2.lags), Sigma=vcov(res2.lags), Terms=2:4)
#Wald test ist not significant, so we have a dynamically complete model, wich is a condition required for good forecasting.

Stock <-newdata2[,"Stock"]
adf.test(Stock)
#probably stationary (10% significance level)
#However, stationarity is not required for the asymptotic properties of OLS

#Breusch-Pagan Test: Null is homosk.
bptest(Stock~Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=newdata2)

#adjusting for heterosk.
coeftest(lin.model, vcovHC)
