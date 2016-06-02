setwd("C:/Users/marcu/Downloads")

data <- read.csv2("~/Dataset FINAL(paperstyle).csv", stringsAsFactors=FALSE)

View(data)

library(foreign)

library(plm) 

library(car)

library(lmtest)   

library(sandwich)

data <- pdata.frame(data, index = c("Company", "Date"), drop.index = F, row.names = T) 

#fixed effects model
fe <- plm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, model = "within", data=data)
summary(fe)

#random effects model
re <- plm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, model = "random", data=data)
summary(re)

#Hausman test comparing RE and FE
phtest(fe, re)

#test for serial correlation
pbgtest(re)

#test for stationarity
library(tseries)
adf.test(data$Stock, k=2)

#test for heteroskedasticity
library(lmtest)
bptest(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=data, studentize=F)

#heterosk. consistent coefficients
coeftest(re, vcovHC)
coeftest(fe)

fepggls <- pggls(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity +Market+ Oil+Gas+EURUSD, model = "fixed", data=data)
summary(fepggls)