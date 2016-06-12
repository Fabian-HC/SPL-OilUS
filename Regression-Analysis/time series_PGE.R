setwd("C:/Users/marcu/Downloads")

data <- read.csv2("~/Dataset FINAL(paperstyle).csv", stringsAsFactors=FALSE)

library(lmtest)
library(sandwich)
library(dyn) #this package includes extensions to time series applications for basic commands
library(forecast)
library(tseries)
library(aod)
library(foreign)
library(plm) 
library(car)


PGECorp <- subset(data, data$Company==8)
PGECorp<-PGECorp[,-1]
#Since we are working with Time Series now, convert the data to a time series object
PGECorp <- ts(PGECorp,start=c(1996,3), end=c(2015,4),frequency=4)

plot(PGECorp[,"Stock"], type="l", lwd=2, col="red", xlab="Date", ylab="Stock")

lin.model.PGE <- lm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=PGECorp)
summary(lin.model.PGE)                   
plot(lin.model.PGE)
outlierTest(lin.model.PGE)
influence.measures(lin.model.PGE)
PGECorp<-PGECorp[-c(58,60),]

dwtest(lin.model.PGE)

#plot(ApacheCorp[,"Stock"])
#plot(PGECorp[,"Stock"])
#ccf(PGECorp[,"Stock"],ApacheCorp[,"Stock"])
#cor.test(PGECorp[,"Stock"],ApacheCorp[,"Stock"])
#joint significance of ind. variables
vcov(lin.model.PGE)
wald.test(b=coef(lin.model.PGE), Sigma=vcov(lin.model.PGE),Terms=1:7)

res.p <- as.ts(lin.model.PGE$residuals) #extract the residuals from the model, and convert them into a time series
#looking at res.
plot(res.p)
lin.model3 <- dyn$lm(Stock ~ Market, data=PGECorp)
summary(lin.model3)
Stock2<-PGECorp[,"Stock"]
Market2<-PGECorp[,"Market"]
ts.plot(Stock2,type="p")
#have a look at autocorrelation in the residuals, using the acf() function
acf(res.p,50)
acf(res.p,50,plot=F)
#If the residuals WERE serially correlated, the Gauss-Markov Assumptions do not hold and ols is not BLUE

#and at partial autocorrelations
pacf(res.p,50)

#regress the residuals of on their lags of -1 to -4 (4 quarters) (so we try to explain residuals by time-shifted residuals, e.g. resid. in t=2 by resid. in t=1)
resa.lags2 <-dyn$lm(res.p~lag(res.p,-1)+lag(res.p,-2)+lag(res.p,-3)+lag(res.p,-4))
summary(resa.lags2)
#test for joint significance of the montly lagged residuals (Breusch-Godfrey Test)
vcov(resa.lags2)
wald.test(b=coef(resa.lags2), Sigma=vcov(resa.lags2), Terms=2:5)
#significant, which does indicate serial correlation

#If we HAD detected serial corr.--> Respecified Model to make it dynamically complete, by including lags of the dependent variable:
#Dynamically complete models should not have any serial correlation in the error term

#1 lag:
dyn.complete3 <- dyn$lm(Stock~lag(Stock,-1)+Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=PGECorp)
summary(dyn.complete3)
#lag 2 not significant => model w/o lag 2 already dynamically complete

#If we wanted to get a complete model through lags --> Again we have to test: Serial correlation in residuals?
res.3 <- as.ts(dyn.complete3$residuals) 
res3.lags <-dyn$lm(res.3~lag(res.3,-1)+lag(res.3,-2)+lag(res.3,-3)+lag(res.3,-4))
summary(res3.lags)
wald.test(b=coef(res3.lags), Sigma=vcov(res3.lags), Terms=2:5)
#Wald test ist not significant, so we have a dynamically complete model, wich is a condition required for good forecasting.
acf(res.3,50)


Stock2 <-PGECorp[,"Stock"]
adf.test(Stock2)
#stationary
#However, stationarity is not required for the asymptotic properties of OLS

#Breusch-Pagan Test: Null is homosk.(which is the result with the lag)
bptest(Stock~lag(Stock,-1)+Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=PGECorp)

#adjusting for heterosk.(just testing)
coeftest(lin.model.PGE, vcovHC)


full.model <- lm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=PGECorp)
reduced.model <- step(full.model, direction="backward")

