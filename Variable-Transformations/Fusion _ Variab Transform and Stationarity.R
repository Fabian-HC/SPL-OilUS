# remove variables
rm(list = ls())

# reset graphics
graphics.off()



# Install packages if not installed
libraries = c("plyr","dplyr","data.table", "tseries", "xtable", "plm", "CADFtest")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
# Package loading done - variable can be deleted
rm(libraries)

# Define required functions

logreturnsfun = function(x){
  n = length(x)
  logreturn = diff(log(x), lag=1)
  # AS numeric part added
}

firstdiffun = function(x){
  n = length(x)
  firstdif = diff(x, lag=1)
}


# Stationarity Test - ADF TEst
stattestFUN = function(x){
  test = adf.test(x, alternative = "stationary")
  testresult = test$p.value
  testresult = round(testresult, digits = 2)
  ReturnResult = if(testresult < 0.1){
    ReturnResult = "stationary"
  }else if(testresult > 0.1 & testresult < 0.2){
    ReturnResult = "Critical a[0.1,0.2]" 
  }else{
    ReturnResult ="not_stationary"
  }
}

# Stationarity Test: KPSS Test
stattestFUN2 = function(x){
  test = kpss.test(x, null = "Trend")
  test = test$p.value
  test = as.numeric(test)
  test = round(test, digits = 2)
  ReturnResult = if(test < 0.099){
    ReturnResult = "non (trend) stationary"
  }else{
    ReturnResult ="(trend) stationary"
  }
}

# Antton's Working directory
# setwd("/Users/antton/Desktop/Statistique programming")
# data = read.csv2("Dataset-FINALupdated_absolute.csv", stringsAsFactors=FALSE)

# Tim's working directory
setwd("C:/Users/Trimme/Documents/GitHub/R_Project/SPL-OilUS")
data = read.csv2("~/GitHub/R_Project/SPL-OilUS/Data-Set/Dataset-FINALupdated_absolute.csv", stringsAsFactors = FALSE)


# Convert Date such that R recognizes it as date
class(data$Date)
data$Date <- as.Date(data$Date, format = "%d.%m.%Y")
class(data$Date)

#Change data order to us Logreturns
data = data[order(data$Company,data$Date),]
# original dataset not needed anymore

# Save the transformed and adequately formatted dataset for my purposes later on
# Note that panel data transformation has not been applied on this data set so far
save(data, file="~/GitHub/R_Project/SPL-OilUS/Data-Set/InitialData_Date_OK.RData")
rm(data)
load(file="~/GitHub/R_Project/SPL-OilUS/Data-Set/InitialData_Date_OK.RData", verbose = TRUE)
# Perform stationarity Tests on the absolute level

data <- pdata.frame(data, index = c("Company", "Date"), drop.index = FALSE, row.names = TRUE)
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
class(data$Date)


CompAssets = read.csv2("~/GitHub/R_Project/SPL-OilUS/Data-Set/Company_TotAssets.csv", stringsAsFactors = FALSE)
class(CompAssets[,3])

save(data, file="~/GitHub/R_Project/SPL-OilUS/Data-Set/InitialData_Panel_Date_OK.RData")

levels(data$Company) = c("Exxon_Mobil", "Apache", "CPEnergy", "Chevron", "Hess_Corp", "Murphy_Oil", "Occidental_Petroleum", "PG&E_Corp", "Williams")

save(data, file="~/GitHub/R_Project/SPL-OilUS/Data-Set/InitialData_Panel_Date_OK_Companynames.RData")

rm(data)

load(file="~/GitHub/R_Project/SPL-OilUS/Data-Set/InitialData_Panel_Date_OK_Companynames.RData")
class(data$Date) # The date variable is indeed read in as date

CompAssets = read.csv2("~/GitHub/R_Project/SPL-OilUS/Data-Set/Company_TotAssets.csv", stringsAsFactors = FALSE)
CompAssets$Company = factor(CompAssets$Company)
levels(CompAssets$Company) = c("Exxon_Mobil", "Apache", "CPEnergy", "Chevron", "Hess_Corp", "Murphy_Oil", "Occidental_Petroleum", "PG&E_Corp", "Williams")
CompAssets$Date <- as.Date(CompAssets$Date, format = "%d.%m.%Y")
class(CompAssets$Date)

data = merge(data, CompAssets)
data$Net.Income = data$Net.Income / data$Assets
data$Assets = NULL

names(data) = gsub("Net.Income", "NI_by_Assets", names(data))
rm(CompAssets)

colnames(data)

# Sort the dataframe before applying transformation
data = data[order(data$Company, data$Date),]

save(data, file = "~/GitHub/R_Project/SPL-OilUS/Data-Set/InitialData_Panel_Date_OK_Companynames_NI_A.RData")
rm(data)
load("~/GitHub/R_Project/SPL-OilUS/Data-Set/InitialData_Panel_Date_OK_Companynames_NI_A.RData")

# Stationarity Test for common factors - ADF Test
Sub1 = subset(data, data$Company == levels(data$Company)[1])
Sub1 = Sub1[,8:11]
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_CommonFactors_Absolute_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_Test_CommonFactors_Absolute_ADF.txt")

# KPSS-Test
testresult2 = apply(Sub1, MARGIN = 2, FUN = stattestFUN2)
testresult2 = data.frame(testresult2)
write.csv2(testresult2, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_CommonFactors_Absolute_KPSS.csv") # csv table export
print.xtable(xtable(testresult2, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_Test_CommonFactors_Absolute_KPSS.txt")
rm(Sub1, testresult, testresult2)


# Stationarity Test for company-specific factors - ADF Test
testresult = aggregate(data[,3:7], by = list(data$Company), 
                       FUN = stattestFUN, simplify = FALSE)
testresult = testresult[,-1]
testresult <- do.call("rbind", lapply(testresult, as.data.frame))
colnames(testresult) = levels(data$Company) 
testresult = as.data.frame(t(testresult))
class(testresult)
# Company names in header are still missing
write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity__Absolute_Company_Specific_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_Test_Absolute_Company-Specific_ADF.txt")


# Statioinarity Test for company-specific factors - KPSS Test
testresult2 = aggregate(data[,3:7], by = list(data$Company), FUN = stattestFUN2, simplify = FALSE)
testresult2 = testresult2[,-1]
testresult2 <- do.call("rbind", lapply(testresult2, as.data.frame)) 
colnames(testresult2) = levels(data$Company)
testresult2 = as.data.frame(t(testresult2))
write.csv2(testresult2, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity__Absolute_Company_Specific_KPSS.csv") # csv table export
print.xtable(xtable(testresult2, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_Test_Absolute_Company-Specific_KPSS.txt")

# Perform a panel unit root test
object <- as.data.frame(split(data[,3:11], data$Company))
class(object)
PanelUnitRootTest = purtest(object = object, test = "levinlin", exo = "trend", lags = "AIC", pmax = 5)
sink(file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity__Absolute_Panel_Test.txt")
PanelUnitRootTest$statistic
sink()

rm(object, testresult ,testresult2, PanelUnitRootTest)




# All test results point to that our variables exhibit non-stationarity!
# Let's apply transformations in the hope to get stationary variables
#logreturn#
LogR = apply (
  X = data[,c(3, 4, 6, 8:11)], 
  MARGIN = 2 , 
  logreturnsfun
)

#first difference
FirstDiff = apply (
  X = data[,c(5,7)], 
  MARGIN = 2 , 
  firstdiffun
)

#deleting false log return#
Datatrans = data.frame(cbind(data[2:711,1:2],LogR,FirstDiff))
Datatrans2 = subset(Datatrans, as.Date("1996-06-30")<Datatrans$Date)
#checking wich row have been deleted
anti_join(Datatrans,Datatrans2)
#final changes#
dataFinal = Datatrans2[,c(1:4,10,5,11,6:9)]

dataFinal <- pdata.frame(dataFinal, index = c("Company", "Date"), drop.index = FALSE, row.names = TRUE)
class(dataFinal$Date)
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
class(data$Date)

# Save the transformed dataset (if you save it as follow Date remanes Date)
# Antton's save place
# Note that panel data transformation has not been applied to this data set so far
# save(dataFinal, file="TransformedData.RData")
save(dataFinal, file="~/GitHub/R_Project/SPL-OilUS/Data-Set/TransformedDate.RData")
rm(data, dataFinal, Datatrans2, Datatrans, LogR, Datatrans, FirstDiff)
load("~/GitHub/R_Project/SPL-OilUS/Data-Set/TransformedDate.RData", verbose = TRUE)
#write.csv2(dataFinal, file = "Z_Transformed_variables_Quarterly_returns_as_Marcus.csv")

# aggregate(dataFinal$NI_by_Assets, by = list(dataFinal$Company), FUN = quantile)

# First apply the unit root panel data test to see whether the overall panel
# is stationary
object <- as.data.frame(split(dataFinal[,3:11], dataFinal$Company))
PanelUnitRootTest = purtest(object = object, test = "levinlin", exo = "trend", lags = "AIC", pmax = 5)
sink(file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity__Return_Panel_Test.txt")
PanelUnitRootTest$statistic
sink()
# result: According to this test, the variables in our panel are stationary

# This test would be cool, yet I cannot convert the object into a pseries object
# cipstest(dataFinal[,3], lags = 2, type = "none", model = "cmg", truncated = FALSE)


# Stationarity Test for common factors
Sub1 = subset(dataFinal, dataFinal$Company == levels(dataFinal$Company)[1])
Sub1 = Sub1[,8:11]
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
# all values ok - all stationary
write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_CommonFactors_Returns_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_Test_Returns_CommonFactors_ADF.txt")
# Dataset Sub1 not needed now - discard it

testresult2 = apply(Sub1, MARGIN = 2, FUN = stattestFUN2)
testresult2 = data.frame(testresult2)
# Note that H1: non-trend-stationarity of EUR/USD is accepted for EUR/USD at 10%,
# but not at the 5% level
write.csv2(testresult2, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_CommonFactors_Returns_KPSS.csv") # csv table export
print.xtable(xtable(testresult2, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_Test_Returns_CommonFactors_KPSS.txt")
# Dataset Sub1 not needed now - discard it
rm(Sub1, testresult, testresult2)

# Stationarity Test for company-specific factors as returns - ADF Test
testresult = aggregate(dataFinal[,3:7], by = list(dataFinal$Company), FUN = stattestFUN, simplify = FALSE)
testresult = testresult[,-1]
testresult <- do.call("rbind", lapply(testresult, as.data.frame)) 
colnames(testresult) = levels(dataFinal$Company) 
testresult = as.data.frame(t(testresult))
write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity__Returns_Company_Specific_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_Test_Returns_Company-Specific_ADF.txt")


# Stationarity Test for company-specific factors as returns - KPSS Test
testresult2 = aggregate(dataFinal[,3:7], by = list(dataFinal$Company), FUN = stattestFUN2, simplify = FALSE)
testresult2 = testresult2[,-1]
testresult2 <- do.call("rbind", lapply(testresult2, as.data.frame)) 
colnames(testresult2) = levels(dataFinal$Company) 
testresult2 = as.data.frame(t(testresult2))
write.csv2(testresult2, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity__Returns_Company_Specific_KPSS.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_Test_Returns_Company-Specific_KPSS.txt")




# -----------------------------------------------------------------
# -----------------------------------------------------------------
# OLD CODE WILL NOT BE USED
# Set a cutoff at 2012

# Watch out - this only works if data Final is a data frame and not a pdata frame
dataFinal = subset(dataFinal, as.Date("2013-03-28")>dataFinal$Date)

object <- as.data.frame(split(dataFinal[,3:11], dataFinal$Company))
class(object)
PanelUnitRootTest = purtest(object = object, test = "levinlin", exo = "trend", lags = "AIC", pmax = 5)
sink(file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity__Return_Panel_Test_Cut2012.txt")
PanelUnitRootTest$statistic
sink()

# Stationarity Test for common factors as returns - ADF
Sub1 = subset(dataFinal, dataFinal$Company == levels(dataFinal$Company)[1])
Sub1 = Sub1[,8:11]
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_CommonFactors_Returns_Cut2012_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_Test_CommonFactors_Returns_Cut2012_ADF.txt")
# Dataset Sub1 not needed now - discard it

# Stationarity Test for common factors as returns - KPSS
testresult2 = apply(Sub1, MARGIN = 2, FUN = stattestFUN2)
testresult2 = data.frame(testresult2)
write.csv2(testresult2, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_CommonFactors_Returns_Cut2012_KPSS.csv") # csv table export
print.xtable(xtable(testresult2, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_Test_CommonFactors_Returns_Cut2012KPSS.txt")


rm(Sub1, testresult, testresult2)

# Stationarity Test for company-specific factors - ADF
testresult = aggregate(dataFinal[,3:7], by = list(dataFinal$Company), FUN = stattestFUN, simplify = FALSE)
testresult = data.frame(testresult[,-1])
testresult <- do.call("rbind", lapply(testresult, as.data.frame))
colnames(testresult) = levels(dataFinal$Company)
testresult = as.data.frame(t(testresult))
# Most stationarity problems drop out when we exclude the years 2013 - 2015
write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_Test_Returns_Company-Specific_Cut2012_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_Returns_Company-Specific_Cut2012_ADF.txt")


# Stationarity Test for company-specific factors - ADF
testresult2 = aggregate(dataFinal[,3:7], by = list(dataFinal$Company), FUN = stattestFUN2, simplify = FALSE)
testresult2 = data.frame(testresult2[,-1])
testresult2 <- do.call("rbind", lapply(testresult2, as.data.frame))
colnames(testresult2) = levels(dataFinal$Company) 
testresult2 = as.data.frame(t(testresult2))
# Most stationarity problems drop out when we exclude the years 2013 - 2015
write.csv2(testresult2, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations_Tim/Stationarity_Test_Returns_Company-Specific_Cut2012_KPSS.csv") # csv table export
print.xtable(xtable(testresult2, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_Returns_Company-Specific_Cut2012_KPSS.txt")

# Save dataset with cutoff 2012
save(dataFinal, file="~/GitHub/R_Project/SPL-OilUS/Data-Set/Stationarity_2012.RData")

rm(dataFinal, testresult, testresult2, object, PanelUnitRootTest)

# ----------
# Try to test whether all variables are stationary in the period 2012 - 2015
load("~/GitHub/R_Project/SPL-OilUS/Data-Set/TransformedDate.RData", verbose = TRUE)
class(dataFinal$Date)
dataFinal = subset(dataFinal, '2012-12-31'<dataFinal$Date)

# Stationarity Test for common factors
Sub1 = subset(dataFinal, dataFinal$Company == levels(dataFinal$Company)[1])
Sub1 = Sub1[,8:11]
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
# all values ok - all stationary
#write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_CommonFactors_Returns_Post2012.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_CommonFactors_Returns_Post2012.txt")
# Dataset Sub1 not needed now - discard it
rm(Sub1, testresult)

# Stationarity Test for company-specific factors
testresult = aggregate(dataFinal[,3:7], by = list(dataFinal$Company), FUN = stattestFUN, simplify = FALSE)
testresult = data.frame(testresult[,-1])
class(testresult[,1])
# Most stationarity problems drop out when we exclude the years 2013 - 2015
# write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_Specific_Returns_Post2012.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_Specific_Returns_Post2012.txt")

# The takeaway from this test session is that the retu

Sub1 = subset(dataFinal, dataFinal$Company == 1)
plot(Sub1$Date, Sub1$Stock, type = "l")
acf(Sub1$Stock)
pacf(Sub1$Stock)
