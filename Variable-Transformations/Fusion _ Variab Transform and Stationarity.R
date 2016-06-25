# YMCMB

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

getwd()
# Tim's working directory
data = read.csv2("./Data-Set/Dataset-FINALupdated_absolute.csv", stringsAsFactors = FALSE)

# Convert Date such that R recognizes it as date
class(data$Date)
data$Date <- as.Date(data$Date, format = "%d.%m.%Y")
class(data$Date)

# Add the variable Assets to our dataset in order to obtain
# Net Income over Assets: NI / A 
CompAssets = read.csv2("./Data-Set/Company_TotAssets.csv", stringsAsFactors = FALSE)
CompAssets$Date <- as.Date(CompAssets$Date, format = "%d.%m.%Y")
class(CompAssets$Date)
# If the classes of the Company indicator match - we can continue
class(data$Company) == class(CompAssets$Company)

# merge the two datasets
data = merge(data, CompAssets)
data$Net.Income = data$Net.Income / data$Assets
data$Assets = NULL
names(data) = gsub("Net.Income", "NI_by_Assets", names(data))
# remove the dataset "CompAssets" - not necessary anymore
rm(CompAssets)
# Are all variables there that we need (17 variables)?
length(colnames(data)) == 17


# Sort the dataframe before applying transformation
data = data[order(data$Company, data$Date),]

# Attach companyNames to the data set
data$Company = as.factor(data$Company)
levels(data$Company) = c("Exxon_Mobil", "Apache", "CPEnergy", "Chevron", "Hess_Corp", "Murphy_Oil", "Occidental_Petroleum", "PG&E_Corp", "Williams")
levels(data$Company)

save(data, file="./Data-Set/InitialData_Panel_Date_OK_Companynames_NI_A.RData")

# ----------------------------------------------------------------
# Stationarity Tests for absolute Variables
# ----------------------------------------------------------------

# Stationarity Test for common factors - ADF Test
Sub1 = subset(data, data$Company == levels(data$Company)[1])
Sub1 = Sub1[,8:11]
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
write.csv2(testresult, file = "./Stationarity-Tests/Stationarity_CommonFactors_Absolute_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "./Stationarity-Tests/Stationarity_Test_CommonFactors_Absolute_ADF.txt")

# KPSS-Test
testresult2 = apply(Sub1, MARGIN = 2, FUN = stattestFUN2)
testresult2 = data.frame(testresult2)
write.csv2(testresult2, file = "./Stationarity-Tests/Stationarity_CommonFactors_Absolute_KPSS.csv") # csv table export
print.xtable(xtable(testresult2, auto = TRUE), file = "./Stationarity-Tests/Stationarity_Test_CommonFactors_Absolute_KPSS.txt")
rm(Sub1, testresult, testresult2)


# Stationarity Test for company-specific factors - ADF Test
testresult = aggregate(data[,3:7], by = list(data$Company), 
                       FUN = stattestFUN, simplify = FALSE)
testresult = testresult[,-1]
testresult <- do.call("rbind", lapply(testresult, as.data.frame))
colnames(testresult) = levels(data$Company) 
testresult = as.data.frame(t(testresult))
class(testresult)
write.csv2(testresult, file = "./Stationarity-Tests/Stationarity__Absolute_Company_Specific_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "./Stationarity-Tests/Stationarity_Test_Absolute_Company-Specific_ADF.txt")


# Statioinarity Test for company-specific factors - KPSS Test
testresult2 = aggregate(data[,3:7], by = list(data$Company), FUN = stattestFUN2, simplify = FALSE)
testresult2 = testresult2[,-1]
testresult2 <- do.call("rbind", lapply(testresult2, as.data.frame)) 
colnames(testresult2) = levels(data$Company)
testresult2 = as.data.frame(t(testresult2))
write.csv2(testresult2, file = "./Stationarity-Tests/Stationarity__Absolute_Company_Specific_KPSS.csv") # csv table export
print.xtable(xtable(testresult2, auto = TRUE), file = "./Stationarity-Tests/Stationarity_Test_Absolute_Company-Specific_KPSS.txt")

# Perform a panel unit root test
object <- as.data.frame(split(data[,3:11], data$Company))
class(object)
PanelUnitRootTest = purtest(object = object, test = "levinlin", exo = "trend", lags = "AIC", pmax = 5)
sink(file = "./Stationarity-Tests/Stationarity__Absolute_Panel_Test.txt")
PanelUnitRootTest$statistic
sink()

rm(object, testresult ,testresult2, PanelUnitRootTest)

# -----------------------------------------------------------------
# Apply stationarity Transformations in form of log returns and first differences
#logreturn#
LogR = apply (
  X = data[,c(3, 4, 6, 8:17)], 
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
dataFinal = Datatrans2[,c(1:4 ,16 ,5 ,17 , 6:15)]
class(dataFinal)
class(dataFinal$Date)

# Remove auxiliary storage variables
rm(Datatrans2, Datatrans, LogR, Datatrans, FirstDiff)
# Note that our data is still not a pdata frame. 
# This will be done RIGHT BEFORE THE REGRESSIONS
save(dataFinal, file="./Data-Set/For_Marcus_OK_2.RData")
data = dataFinal
rm(dataFinal)


# First apply the unit root panel data test to see whether the overall panel
# is stationary
object <- as.data.frame(split(data[,3:11], data$Company))
PanelUnitRootTest = purtest(object = object, test = "levinlin", exo = "trend", lags = "AIC", pmax = 5)
sink(file = "./Stationarity-Tests/Stationarity__Return_Panel_Test.txt")
PanelUnitRootTest$statistic
sink()
rm(object)
# result: According to this test, the variables in our panel are stationary

# Stationarity Test for common factors- ADF
Sub1 = subset(data, data$Company == levels(data$Company)[1])
Sub1 = Sub1[,8:11]
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
# all values ok - all stationary
write.csv2(testresult, file = "./Stationarity-Tests/Stationarity_CommonFactors_Returns_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "./Stationarity-Tests/Stationarity_Test_Returns_CommonFactors_ADF.txt")

testresult2 = apply(Sub1, MARGIN = 2, FUN = stattestFUN2)
testresult2 = data.frame(testresult2)
# Note that H1: non-trend-stationarity of EUR/USD is accepted for EUR/USD at 10%,
# but not at the 5% level
write.csv2(testresult2, file = "./Stationarity-Tests/Stationarity_CommonFactors_Returns_KPSS.csv") # csv table export
print.xtable(xtable(testresult2, auto = TRUE), file = "./Stationarity-Tests/Stationarity_Test_Returns_CommonFactors_KPSS.txt")
# Dataset Sub1 not needed now - discard it
rm(Sub1, testresult, testresult2)

# Stationarity Test for company-specific factors as returns - ADF Test
testresult = aggregate(data[,3:7], by = list(data$Company), FUN = stattestFUN, simplify = FALSE)
testresult = testresult[,-1]
testresult <- do.call("rbind", lapply(testresult, as.data.frame)) 
colnames(testresult) = levels(data$Company) 
testresult = as.data.frame(t(testresult))
write.csv2(testresult, file = "./Stationarity-Tests/Stationarity__Returns_Company_Specific_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "./Stationarity-Tests/Stationarity_Test_Returns_Company-Specific_ADF.txt")


# Stationarity Test for company-specific factors as returns - KPSS Test
testresult2 = aggregate(data[,3:7], by = list(data$Company), FUN = stattestFUN2, simplify = FALSE)
testresult2 = testresult2[,-1]
testresult2 <- do.call("rbind", lapply(testresult2, as.data.frame)) 
colnames(testresult2) = levels(data$Company) 
testresult2 = as.data.frame(t(testresult2))
write.csv2(testresult2, file = "./Stationarity-Tests/Stationarity__Returns_Company_Specific_KPSS.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "./Stationarity-Tests/Stationarity_Test_Returns_Company-Specific_KPSS.txt")

rm(list = ls())
