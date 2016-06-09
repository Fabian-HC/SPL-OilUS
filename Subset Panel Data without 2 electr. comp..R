data <- read.csv2("~/Dataset FINAL(paperstyle).csv", stringsAsFactors=FALSE)

#exclude electr. companies(3&8)
Subsetdata <- subset(data, data$Company!=3 & data$Company!=8)
Subsetdata$Date<-as.Date(as.character(Subsetdata$Date), "%d.%m.%Y")

Subsetdata <- pdata.frame(Subsetdata, index = c("Company", "Date"), drop.index = F, row.names = T)

#fixed effects model
fe2 <- plm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, model = "within", data=Subsetdata)
summary(fe2)

#random effects model
re2 <- plm(Stock ~ Assets.to.Market.Cap +Net.Income + BV.Equity.to.Market.Cap + Debt.to.Equity+Oil+Gas+Market+EURUSD, model = "random", data=Subsetdata)
summary(re2)

#Hausman test comparing RE and FE
phtest(fe2, re2)