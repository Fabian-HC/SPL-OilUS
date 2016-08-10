# === Clear the screen ===
cat("\014")

# Tested With: 
# R version 3.2.4 Revised (2016-03-16 r70336) -- "Very Secure Dishes"
# Copyright (C) 2016 The R Foundation for Statistical Computing
# Platform: i386-w64-mingw32/i386 (32-bit)


# === Clearing the Environment === remove variables
rm(list = ls())

# reset graphics
graphics.off()

# === Set adequate working directory ===
setwd("~/GitHub/Test/SPL-OilUS_2/SubsampleAnalysis")

# === Packages === Install packages if not installed
libraries = c("stargazer", "tseries", "plm", "car", "xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
       install.packages(x)})
# Load packages
lapply(libraries, 
      library, 
      quietly        = TRUE, 
      character.only = TRUE)

# === Load Data Set === load variable
load("./RegressionBase2.RData")

data0 = data[!(data$Company == "CPEnergy" | data$Company == "PG&E_Corp"), ]
class(data$Company)

#function to prepare the variable for result exportation
Signif = function(a) {
                      mat                      = summary(a)$coefficients
                      mat                      = mat[, c(1, 4)]
                      signif                   = rep("", dim(mat)[1])
                      signif[mat[, 2] < 0.10]  = "*"
                      signif[mat[, 2] < 0.05]  = "**"
                      signif[mat[, 2] < 0.01]  = "***"
                      mat                      = as.data.frame(mat)
                      mat[, 2]                 = signif
                      names(mat)               = c("Estimate", "")
                      mat[, 1]                 = round(mat[, 1], 2)
                      return(mat)}
# function to build the result table when there is only one model => used in written report
ResultLatex1 = function(a) {
                             mat = Signif(a)
                             return (xtable(mat))}
# function to build the result table when there are two models => used in oral presentation
ResultLatex2 = function(a, b) {
                             mata = Signif(a)
                             matb = Signif(b)
                             mat  = cbind(mata, matb)
                             return (xtable(mat)) }

# =============== BEFOR / AFTER 2008 ===============#

# subsetting before and after 2008 #
Pre2008   = pdata.frame(subset(data0, 
                               Date <= "2008-08-31"), 
                       index      = c("Company", "Date"), 
                       drop.index = F, 
                       row.names  = T)
Post2008  = pdata.frame(subset(data0, 
                               Date > "2008-08-31"), 
                       index      = c("Company", "Date"), 
                       drop.index = F, 
                       row.names  = T)

# Running the regression with both subsets#
repre2008 = plm(Stock ~ Oil + Gas + Market + EURUSD, 
                model = "random", 
                data  = Pre2008)

rePost2008 = plm(Stock ~ Oil + Gas + Market + EURUSD, 
                 model = "random", 
                 data  = Post2008)

# results
R2_2008 = ResultLatex2(
                        a = repre2008, 
                        b = rePost2008)
print.xtable(xtable(R2_2008),
             type = "latex", 
             file = "./2R2008.txt", 
             size = "tiny")

# using dummy varibales
# creating dummy#

DumP  = as.numeric(data0$Date > "2008-08-31")
dataP = cbind(data0, 
              `After2008(=1)` = DumP)
dataP = pdata.frame(dataP, 
                    index      = c("Company", "Date"), 
                    drop.index = F, 
                    row.names  = T)

# Random Effect with Dummy 
DumyReg = plm( Stock ~ Oil + Gas + Market + EURUSD + DumP + DumP * Oil + DumP * Gas + DumP * Market + DumP * EURUSD, 
               model = "random", 
               data  = dataP)

D_2008 = ResultLatex1(a = DumyReg)
print.xtable(xtable(D_2008),
             type = "latex", 
             file = "./D_2008.txt", 
             size = "tiny")

#===============  SEASONALITY  ===============#

### creating factor Function for quarter#
quarters = lapply(data0$Date, quarters)
quarter2 = unlist(quarters)

# Data#
dataQ              = cbind(data0, 
                           Quarter = quarter2)
dataQ[, "Quarter"] = factor(dataQ[, "Quarter"])
is.factor(dataQ[, "Quarter"])
dataQ              = pdata.frame(dataQ, 
                                 index      = c("Company", "Date"), 
                                 drop.index = F, 
                                 row.names  = T)

# regression#
Quarter = plm(Stock ~ Oil + Gas + Market + EURUSD + Quarter + 
                      Quarter*Gas + Quarter*Market + Quarter*EURUSD,
              model = "random", 
              data  = dataQ)



D_Quarter = ResultLatex1(a = Quarter)
print.xtable(xtable(D_Quarter),
             type = "latex", 
             file = "./D_Q.txt", 
             size = "tiny")


############### Type of Firm #############

## creation of a dummy such as Oil firm = 0, Other = 1## Carefull use of data with all the firm ##

# subsetting # the first subset has already been created #
OilC   = pdata.frame(data0, 
                     index      = c("Company", "Date"), 
                     drop.index = F, 
                     row.names  = T)

OtherC = pdata.frame(subset(data, 
                            data$Company == "CPEnergy" | data$Company == "PG&E_Corp"), #integration of all the firm #
                            index        = c("Company", "Date"), 
                            drop.index   = F, 
                            row.names    = T)

# Random Effect Regression : Oil Firm
reOilC   = plm(Stock ~ Oil + Gas + Market + EURUSD, 
               model = "random", 
               data  = OilC)
 
# Random Effect Regression : Other firm
reOtherC = plm(Stock ~ Oil + Gas + Market + EURUSD, 
               model = "random", 
               data  = OtherC)

# results
R2_Ftype  = ResultLatex2(a = reOilC, 
                        b = reOtherC)
print.xtable(xtable(R2_Ftype),
             type = "latex", 
             file = "./R2_Ftype.txt", 
             size = "tiny")

############### Regression with a Dummy ################

DumFirmT = ifelse(data$Company == "CPEnergy" | data$Company == "PG&E_Corp", 1, 0)
length(DumFirmT)
DataT = cbind(data, DumFirmT)
DataT = pdata.frame(DataT, 
                    index      = c("Company", "Date"), 
                    drop.index = F, 
                    row.names  = T)

TypeFirm = plm(Stock ~ Oil + Gas + Market + EURUSD + DumFirmT + DumFirmT * Oil + 
                       DumFirmT * Gas + DumFirmT * Market + DumFirmT * EURUSD, 
               model = "random", 
               data  = DataT)

D_Ftype  = ResultLatex1(a = TypeFirm)
print.xtable(xtable(D_Ftype),
             type = "latex", 
             file = "./D_Ftype.txt", 
             size = "tiny")



###### Very strong multicolinearity as result of subsampling ###### We initially ambitioned to anaylis the structural break due to the crisis
###### of 2008 including the specific factors.  However, subsampling increased strongly multicolonearity and some regression could not be run#
###### In the Following section we compute the Variance Inflation Factor (VIF) to quantifie the severity of mutlicollinearity It provide an
###### index than measures how much the variance of an estimated regression coefficient is increased because of collinearity

VIFReg = plm(Stock ~ NI + BVE.MCAP + D.MCAP + Oil + Gas + Market + EURUSD + 
                     DumP * NI + DumP * BVE.MCAP + DumP * D.MCAP + DumP * Oil + 
                     DumP * Gas + DumP * Market, 
             model = "pooling", 
             data  = dataP)
vif(VIFReg)

# In this example we could not compute the Random effect regression including the D.MCAP*DumP regressor due to Multicollinearity# 

#result exportation#
EstimatedVif = as.vector(vif(VIFReg))
EstimatedVif = round(EstimatedVif,2)
Variable     = c("NI","BVE.MCAP" , "D.MCAP" , "Oil" , "Gas" , "Market" , "EURUSD", "DumP",
                 "D*NI" , "D*BVE.MCAP" , "DumP*D.MCAP" , "DumP*Oil" , "DumP*Gas" , "DumP*Market")
mat          = cbind(Variable,EstimatedVif)
mat2         = rbind(t(mat[1:7,]),t(mat[8:14,])) #making it fit for paper
mat2         = as.data.frame(mat2)

print.xtable(xtable(mat2),
             type = "latex", 
             file = "./Vif.txt", 
             size = "tiny")


# Another Example
VIFReg2 = plm(Stock ~ Oil + Gas + Market + EURUSD + Quarter + 
                Quarter*Oil + Quarter*Gas + Quarter*Market + Quarter*EURUSD,
              model = "pooling", 
              data  = dataQ)
vif(VIFReg2)
