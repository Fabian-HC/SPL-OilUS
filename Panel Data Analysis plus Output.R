setwd("C:/Users/marcu/Downloads")

data <- read.csv2("~/Dataset FINAL(paperstyle).csv", stringsAsFactors=FALSE)

View(data)

library(foreign)
library(plm) 
library(car)
library(lmtest)   
library(sandwich)
library(stargazer)

data$Date<-as.Date(as.character(data$Date), "%d.%m.%Y")

data <- pdata.frame(data, index = c("Company", "Date"), drop.index = F, row.names = T) 

#fixed effects model
fe <- plm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, model = "within", data=data)
summary(fe)
stargazer(fe,type="text",title="Oneway (individual) effect Within Model",dep.var.labels=c("Stock return"),covariate.labels=c("Assets over market cap.","Net income","Book value equity over market cap.","Debt over equity","Oil price","Gas price","DJI premium"), out="femodel.txt")

res. <- fe$residuals
plot(res.)
plot(res., type="l", lwd=2, col="red", xlab="Date", ylab="residuals")
acf(res.,700)

#random effects model
re <- plm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, model = "random", data=data)
summary(re)
res.2 <- re$residuals
stargazer(fe,type="text",title="Oneway (individual) effect Random Effect Model (Swamy-Arora's transformation)",dep.var.labels=c("Stock return"),covariate.labels=c("Assets over market cap.","Net income","Book value equity over market cap.","Debt over equity","Oil price","Gas price","DJI premium"), out="remodel.txt")

#Hausman test comparing RE and FE
phtest(fe, re)

#test for serial correlation
pbgtest(fe)

#test for stationarity
library(tseries)
adf.test(data$Stock, k=2)

#test for heteroskedasticity
library(lmtest)
bptest(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=data, studentize=F)

#heterosk. and serial corr. consistent coefficient
coeftest(re, vcovHC(re,method="arellano"))
stargazer(re,type="text",title="Arellano model",dep.var.labels=c("Stock return"),covariate.labels=c("Assets over market cap.","Net income","Book value equity over market cap.","Debt over equity","Oil price","Gas price","DJI premium"), out="arellanomodel.txt")

repggls <- pggls(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity +Market+ Oil+Gas+EURUSD, model = "random", data=data)
summary(repggls)

# Bars at top indicate corresponding graph from left to right starting on the bottom row
coplot(Stock ~ Date|Company, type="l", data=data, xlim=c(1,78))

plot(Stock~Market|data$Company)

# plotmeans draw a 95% confidence interval around the means
library(gplots)
plotmeans(Stock ~ Company, main="Heterogeineity across Companie", data=data)

#Regular OLS regression does not consider heterogeneity across groups or time
lm<-lm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=data)
plot(lm)
yhat <- lm$fitted
plot(data$Market, data$Stock, pch=19, xlab="Market premium", ylab="Stock return")
abline(lm(data$Stock ~ data$Market),lwd=3, col="red")

