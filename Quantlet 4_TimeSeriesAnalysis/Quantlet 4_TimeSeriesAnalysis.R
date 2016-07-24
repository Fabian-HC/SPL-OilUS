# Tested With: 
# R version 3.2.4 Revised (2016-03-16 r70336) -- "Very Secure Dishes"
# Copyright (C) 2016 The R Foundation for Statistical Computing
# Platform: i386-w64-mingw32/i386 (32-bit)


# === Load data and install packages ===
load("./Data-Set/RegressionBase.RData")

# Install packages if not installed
libraries = c("lmtest","sandwich","dyn","forecast","tseries","aod","foreign","plm","car","stargazer", "xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# === Time Series Analysis ===
# company supplying electricity
PGECorp = subset(data, data$Company=="PG&E_Corp")
PGECorp = PGECorp[,-(1:2)]

# Since we are working with Time Series now, convert the data to a time series object
PGECorp = ts(PGECorp,start=c(1996,3), end=c(2015,4),frequency=4)

# Linear model
lin.model.PGE = lm(Stock ~ NI + BVE.MCAP + D.MCAP+Oil+Gas+Market, 
                   data=PGECorp)
summary(lin.model.PGE)                   
plot(lin.model.PGE)

# Test for autocorrelation
dwtest(lin.model.PGE)

# Breusch-Pagan Test: Null is homosk.
bptest(Stock~NI + BVE.MCAP + D.MCAP+Oil+Gas+Market, data=PGECorp)

# Adjusting for heterosk.
coeftest1 = coeftest(lin.model.PGE, vcovHC)
coeftest1

# Different company from oil sector
ApacheCorp = subset(data, data$Company=="Apache") 
ApacheCorp = ApacheCorp[,-(1:2)]

# Since we are working with Time Series now, convert the data to a time series object
ApacheCorp = ts(ApacheCorp,start=c(1996,3), end=c(2015,4),frequency=4)

# Linear model
lin.model.Apache = lm(Stock ~ NI + BVE.MCAP + D.MCAP+Oil+Gas+Market, 
                      data=ApacheCorp)
summary(lin.model.Apache)                   
plot(lin.model.Apache)

# test for autocorrelation
dwtest(lin.model.Apache)

# Breusch-Pagan Test: Null is homosk.(which is the result)
bptest(Stock~NI + BVE.MCAP + D.MCAP+Oil+Gas+Market, data=ApacheCorp)

# Output for LATEX (building a function is not necessary b/c it is being used only once)
mat1                      = summary(lin.model.Apache)$coefficients
mat1                      = mat1[, c(1, 4)]
signif                    = rep("", dim(mat1)[1])
signif[mat1[, 2] < 0.1]   = "*"
signif[mat1[, 2] < 0.05]  = "**"
signif[mat1[, 2] < 0.01]  = "***"
mat1                      = as.data.frame(mat1)
mat1[, 2]                 = signif
names(mat1)               = c("Estimate", "")
mat1[, 1]                 = round(mat1[, 1], 2)
mat2                      = coeftest1[, c(1, 4)]
signif                    = rep("", dim(mat2)[1])
signif[mat2[, 2] < 0.1]   = "*"
signif[mat2[, 2] < 0.05]  = "**"
signif[mat2[, 2] < 0.01]  = "***"
mat2                      = as.data.frame(mat2)
mat2[, 2]                 = signif
names(mat2)               = c("Estimate", "")
mat2[, 1]                 = round(mat2[, 1], 2)
mat3                      = cbind(mat1, mat2)
xtable(mat3)
print(xtable(mat3), type  = "latex", size = "tiny", file = "./Quantlet 4_TimeSeriesAnalysis/timeseries.txt")
