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
class(data$Date)
#data <- pdata.frame(data, index = c("Company", "Date"), drop.index = F, row.names = T)

ApacheCorp <- subset(data, data$Company==2) 
#Since we are working with Time Series now, convert the data to a time series object
ApacheCorp <- ts(ApacheCorp,start=c(1996,3), end=c(2015,4),frequency=4)


plot(ApacheCorp[,"Stock"], type="l", lwd=2, col="red", xlab="Date", ylab="Stock")


lin.model.Apache <- lm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=ApacheCorp)
summary(lin.model.Apache)                   
plot(lin.model.Apache)


#joint significance of ind. variables
vcov(lin.model.Apache)
wald.test(b=coef(lin.model.Apache), Sigma=vcov(lin.model.Apache),Terms=1:7)

res.a <- as.ts(lin.model.Apache$residuals) #extract the residuals from the model, and convert them into a time series
#looking at res.
plot(res.a)
lin.model2 <- dyn$lm(Stock ~ Market, data=ApacheCorp)
summary(lin.model2)
Stock<-ApacheCorp[,"Stock"]
Market<-ApacheCorp[,"Market"]
ts.plot(Stock,type="p")
#have a look at autocorrelation in the residuals, using the acf() function
acf(res.a,50)
acf(res.a,50,plot=F)
#If the residuals WERE serially correlated, the Gauss-Markov Assumptions do not hold and ols is not BLUE

#and at partial autocorrelations
pacf(res.a,50)

#regress the residuals on their lags of -1 to -4 (4 quarters) (so we try to explain residuals by time-shifted residuals, e.g. resid. in t=2 by resid. in t=1)
resa.lags <-dyn$lm(res.a~lag(res.a,-1)+lag(res.a,-2)+lag(res.a,-3)+lag(res.a,-4))
summary(resa.lags)
#test for joint significance of the montly lagged residuals (Breusch-Godfrey Test)
vcov(resa.lags)
wald.test(b=coef(resa.lags), Sigma=vcov(resa.lags), Terms=2:5)
#(in)significant, which does (not) indicate serial correlation

#If we HAD detected serial corr.--> Respecified Model to make it dynamically complete, by including lags of the dependent variable:
#Dynamically complete models should not have any serial correlation in the error term

#1 lag:
dyn.complete2 <- dyn$lm(Stock~lag(Stock,-1)+Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=ApacheCorp)
summary(dyn.complete2)
#lags not significant => model w/o lags already dynamically complete

#If we wanted to get a complete model through lags --> Again we have to test: Serial correlation in residuals?
res.2 <- as.ts(dyn.complete2$residuals) 
res2.lags <-dyn$lm(res.2~lag(res.2,-1)+lag(res.2,-2)+lag(res.2,-3)+lag(res.2,-4))
summary(res2.lags)
wald.test(b=coef(res2.lags), Sigma=vcov(res2.lags), Terms=2:5)
#Wald test is even less significant, so we have a dynamically complete model, wich is a condition required for good forecasting.
acf(res.2,50)

Stock <-ApacheCorp[,"Stock"]
adf.test(Stock)
#stationary
#However, stationarity is not required for the asymptotic properties of OLS

#Breusch-Pagan Test: Null is homosk.(which is the result)
bptest(Stock~Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=ApacheCorp)

#adjusting for heterosk.(just testing)
coeftest(lin.model.Apache, vcovHC)
