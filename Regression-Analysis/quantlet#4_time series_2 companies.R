load("./Data-Set/For_Marcus_OK_Old_Version.RData")

data = Datatrans
rm(dataFinal)
# Install packages if not installed
libraries = c("lmtest","sandwich","dyn","forecast","tseries","aod","foreign","plm","car","stargazer")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# company supplying electricity
PGECorp = subset(data, data$Company=="PG&E_Corp")
PGECorp = PGECorp[,-(1:2)]


# Since we are working with Time Series now, convert the data to a time series object
PGECorp = ts(PGECorp,start=c(1996,3), end=c(2015,4),frequency=4)

#linear model
lin.model.PGE = lm(Stock ~ A.MCAP +NI + BVE.MCAP + D.MCAP+Oil+Gas+Market, 
                   data=PGECorp)
summary(lin.model.PGE)                   
plot(lin.model.PGE)

# output
stargazer(lin.model.PGE,title="lin.model.PGE",dep.var.labels=c("Stock return"),font.size="tiny",
          out="./Regression-Analysis/linmodelPGE.LATEX")

#test for autocorrelation
dwtest(lin.model.PGE)

#Breusch-Pagan Test: Null is homosk.
bptest(Stock~A.MCAP +NI + BVE.MCAP + D.MCAP+Oil+Gas+Market, data=PGECorp)

#adjusting for heterosk.
coeftest1 = coeftest(lin.model.PGE, vcovHC)
summary(coeftest1)

# output
stargazer(coeftest1,title="lin.model.PGE adj. for heterosk.",dep.var.labels=c("Stock return"), font.size="tiny",out="./Regression-Analysis/coeftest1.LATEX")

# different company from oil sector
ApacheCorp = subset(data, data$Company=="Apache") 
ApacheCorp<-ApacheCorp[,-(1:2)]

# Since we are working with Time Series now, convert the data to a time series object
ApacheCorp = ts(ApacheCorp,start=c(1996,3), end=c(2015,4),frequency=4)

# linear model
lin.model.Apache = lm(Stock ~ A.MCAP +NI + BVE.MCAP + D.MCAP+Oil+Gas+Market, 
                      data=ApacheCorp)
summary(lin.model.Apache)                   
plot(lin.model.Apache)

# output
stargazer(lin.model.Apache,title="lin.model.Apache",dep.var.labels=c("Stock return"), out="./Regression-Analysis/linmodelApache.LATEX")

# test for autocorrelation
dwtest(lin.model.Apache)

# Breusch-Pagan Test: Null is homosk.(which is the result)
bptest(Stock~A.MCAP +NI + BVE.MCAP + D.MCAP+Oil+Gas+Market, data=ApacheCorp)

########## old with CSV ############

setwd("C:/Users/marcu/Downloads")

data <- read.csv2("~/Dataset FINAL(paperstyle).csv", stringsAsFactors=FALSE)

# Install packages if not installed
libraries = c("lmtest","sandwich","dyn","forecast","tseries","aod","foreign","plm","car","stargazer")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# company supplying electricity
PGECorp <- subset(data, data$Company==8)
PGECorp<-PGECorp[,-(1:2)]

# Since we are working with Time Series now, convert the data to a time series object
PGECorp <- ts(PGECorp,start=c(1996,3), end=c(2015,4),frequency=4)

#linear model
lin.model.PGE <- lm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market, 
                    data=PGECorp)
summary(lin.model.PGE)                   
plot(lin.model.PGE)

# output
stargazer(lin.model.PGE,title="lin.model.PGE",dep.var.labels=c("Stock return"),covariate.labels=c("Assets over market cap.",
          "Net income","Book value equity over market cap.","Debt over equity","Oil price","Gas price","DJI premium"), 
          out="linmodelPGE.LATEX")

#test for autocorrelation
dwtest(lin.model.PGE)

#Breusch-Pagan Test: Null is homosk.
bptest(Stock~Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market, data=PGECorp)

#adjusting for heterosk.
coeftest1<-coeftest(lin.model.PGE, vcovHC)
summary(coeftest1)

# output
stargazer(coeftest1,title="lin.model.PGE adj. for heterosk.",dep.var.labels=c("Stock return"),
          covariate.labels=c("Assets over market cap.","Net income","Book value equity over market cap.",
          "Debt over equity","Oil price","Gas price","DJI premium"), out="coeftest1.LATEX")

# different company from oil sector
ApacheCorp <- subset(data, data$Company==2) 
ApacheCorp<-ApacheCorp[,-(1:2)]

# Since we are working with Time Series now, convert the data to a time series object
ApacheCorp <- ts(ApacheCorp,start=c(1996,3), end=c(2015,4),frequency=4)

# linear model
lin.model.Apache <- lm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market, 
                      data=ApacheCorp)
summary(lin.model.Apache)                   
plot(lin.model.Apache)

# output
stargazer(lin.model.Apache,title="lin.model.Apache",dep.var.labels=c("Stock return"),
          covariate.labels=c("Assets over market cap.","Net income","Book value equity over market cap.",
          "Debt over equity","Oil price","Gas price","DJI premium"), out="linmodelApache.LATEX")

# test for autocorrelation
dwtest(lin.model.Apache)

# Breusch-Pagan Test: Null is homosk.(which is the result)
bptest(Stock~Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, data=ApacheCorp)
