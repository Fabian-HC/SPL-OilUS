setwd("C:/Users/marcu/Downloads")

data <- read.csv2("~/Dataset FINAL(paperstyle).csv", stringsAsFactors=FALSE)

View(data)

library(foreign)
library(plm) 
library(car)
library(lmtest)   
library(sandwich)
library(stargazer)

#Regular OLS regression does not consider heterogeneity across groups or time
lm<-lm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market, data=data)
plot(lm)
outlierTest(lm)
influence.measures(lm)
data<-data[-c(679,676,678),]

yhat <- lm$fitted
plot(data$Market, data$Stock, pch=19, xlab="Market premium", ylab="Stock return")
abline(lm(data$Stock ~ data$Market),lwd=3, col="red")

#panel data now
data$Date<-as.Date(as.character(data$Date), "%d.%m.%Y")

data <- pdata.frame(data, index = c("Company", "Date"), drop.index = F, row.names = T) 

#fixed effects model
fe <- plm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market, 
          model = "within", data=data)
summary(fe)
stargazer(fe,title="Oneway (individual) effect Within Model",dep.var.labels=c("Stock return"),
          covariate.labels=c("Assets over market cap.","Net income","Book value equity over market cap.",
          "Debt over equity","Oil price","Gas price","DJI premium"), out="femodel.LATEX")

res. <- fe$residuals
plot(res.)
plot(res., type="l", lwd=2, col="red", xlab="Date", ylab="residuals")
acf(res.,20)

#random effects model
re <- plm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market, 
          model = "random", data=data)
summary(re)
res.2 <- re$residuals
stargazer(fe,title="Oneway (individual) effect Random Effect Model (Swamy-Arora's transformation)",
          dep.var.labels=c("Stock return"),covariate.labels=c("Assets over market cap.","Net income",
          "Book value equity over market cap.","Debt over equity","Oil price","Gas price","DJI premium"), out="remodel.LATEX")

#Hausman test comparing RE and FE
phtest(fe, re)

#test for serial correlation
pbgtest(re)

#test for stationarity
library(tseries)
lapply(data[,-(1:2)], adf.test)
plot(data$Debt.to.Equity)

#test for heteroskedasticity
library(lmtest)
bptest(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+factor(Company), 
      data=data, studentize=F)

#heterosk. and serial corr. consistent coefficient
coeftest(re, vcovHC(re,method="arellano"))
stargazer(re,title="Arellano model",dep.var.labels=c("Stock return"),covariate.labels=c("Assets over market cap.",
          "Net income","Book value equity over market cap.","Debt over equity","Oil price","Gas price","DJI premium"), 
          out="arellanomodel.LATEX")

## SCC robust significance test, default
coeftest(re, vcov=vcovSCC)

#GLS
repggls <- pggls(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity +Market+ Oil+Gas, 
                model = "pooling", data=data)
summary(repggls)

# Bars at top indicate corresponding graph from left to right starting on the bottom row
coplot(Debt.to.Equity ~ Date|Company, type="l", data=data, xlim=c(1,78))

# plotmeans draw a 95% confidence interval around the means
library(gplots)
plotmeans(Stock ~ Company, main="Heterogeineity across Companie", data=data)

#cross-sectional dependence
pcdtest(fe, test = c("lm"))
pcdtest(fe, test = c("cd"))
