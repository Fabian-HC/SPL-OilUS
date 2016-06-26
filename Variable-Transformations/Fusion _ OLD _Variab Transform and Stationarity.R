# === Clearing the Environment ===
# remove variables
rm(list = ls())

# reset graphics
graphics.off()


# === Packages ===
# Install packages if not installed
libraries = c("plyr","dplyr","data.table", "tseries", "xtable", 
              "plm", "CADFtest")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
#  Load packages 
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
rm(libraries) # package loading done - not needed anymore

# === Define required functions ===
# Functions for variable transformatin later on
logreturnsfun = function(x){
  n = length(x)
  logreturn = diff(log(x), lag=1)
  # AS numeric part added
}

scalefun = function(x){
  ScaleVar = scale(x)
}


# Function for seperating the common factor developments apart from the others
ComFSep = function(x){
  if(class(x) == "data.frame"){
    Sub1 = subset(x, x$Company == levels(x$Company)[1])
    Sub1 = Sub1[,c("Oil", "Gas", "Market", "EURUSD")]
    # print("continue programming")
    return(Sub1)
  }else{
    print("Data must be passed as data frame or a subsection of it")
  }
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


# ======= Initial data modifications =====
# loading data
data = read.csv2("./Data-Set/Dataset-FINALupdated_absolute_2.csv", 
                 stringsAsFactors = FALSE)

# Convert Date such that R recognizes it as date
class(data$Date)
data$Date <- as.Date(data$Date, format = "%d.%m.%Y")
class(data$Date)

# Order data adequately for subsequent transformations
data = data[order(data$Company, data$Date),]


# Set shorter ColumnNames for subsequent analysis
colnames(data) = c("Date", "Company", "Stock", "A-MCAP", 
                   "BVE-MCAP", "D-MCAP", "NI", colnames(data[8:ncol(data)])) 

# Attach companyNames to the data set
data$Company = as.factor(data$Company) # Company variable from integer to factor
levels(data$Company) = c("Exxon_Mobil", "Apache", 
                         "CPEnergy", "Chevron", "Hess_Corp", "Murphy_Oil", 
                         "Occidental_Petroleum", "PG&E_Corp", "Williams")

# Save the dataset - used by graphics quantlet
save(data, file="./Data-Set/InitialData_Panel_Date_OK_OLD_ZScore.RData")

# === Stationarity Tests for absolute Variables ===

# Stationarity Test for common factors - ADF Test
Sub1 = ComFSep(data)
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
write.csv2(testresult, 
           file = "./Stationarity-Tests/Z-Score_Stationarity_CommonFactors_Absolute_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), 
             file = "./Stationarity-Tests/Z-Score_Stationarity_Test_CommonFactors_Absolute_ADF.txt")

# KPSS-Test
testresult2 = apply(Sub1, MARGIN = 2, FUN = stattestFUN2)
testresult2 = data.frame(testresult2)
write.csv2(testresult2, file = "./Stationarity-Tests/Z-Score_Stationarity_CommonFactors_Absolute_KPSS.csv") # csv table export
print.xtable(xtable(testresult2, auto = TRUE), 
             file = "./Stationarity-Tests/Z-Score_Stationarity_Test_CommonFactors_Absolute_KPSS.txt")
rm(Sub1, testresult, testresult2)


# Stationarity Test for company-specific factors - ADF Test
testresult = aggregate(data[,3:7], by = list(data$Company), 
                       FUN = stattestFUN, simplify = FALSE)
testresult = testresult[,-1]
testresult <- do.call("rbind", lapply(testresult, as.data.frame))
colnames(testresult) = levels(data$Company) 
testresult = as.data.frame(t(testresult))
write.csv2(testresult, file = "./Stationarity-Tests/Z-Score_Stationarity__Absolute_Company_Specific_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "./Stationarity-Tests/Z-Score_Stationarity_Test_Absolute_Company-Specific_ADF.txt")
rm(testresult)

# Statioinarity Test for company-specific factors - KPSS Test
testresult2 = aggregate(data[,3:7], by = list(data$Company), FUN = stattestFUN2, simplify = FALSE)
testresult2 = testresult2[,-1]
testresult2 <- do.call("rbind", lapply(testresult2, as.data.frame)) 
colnames(testresult2) = levels(data$Company)
testresult2 = as.data.frame(t(testresult2))
write.csv2(testresult2, file = "./Stationarity-Tests/Z-Score_Stationarity__Absolute_Company_Specific_KPSS.csv") # csv table export
print.xtable(xtable(testresult2, auto = TRUE), file = "./Stationarity-Tests/Z-Score_Stationarity_Test_Absolute_Company-Specific_KPSS.txt")
rm(testresult2)

# Perform a panel unit root test
object <- as.data.frame(split(data[,3:18], data$Company))
class(object)
PanelUnitRootTest = purtest(object = object, test = "levinlin", 
                            exo = "trend", lags = "AIC", pmax = 5)
sink(file = "./Stationarity-Tests/Z-Score_Stationarity__Absolute_Panel_Test.txt")
PanelUnitRootTest$statistic
sink()

rm(object, PanelUnitRootTest)

# === Apply (Stationarity) Transformations ==
# Apply log - transformation to variable
dataHelp = cbind(data[,1:2], log(data[, c(4,6)]), data[,c(18)])
colnames(dataHelp) = colnames(data[,c(1:2, 4,6, 18)])
# else:  "A-MCAP","D-MCAP"

# Apply log-return transformation to predefinded variable set
LogR = apply (
  X = data[,c(3, 8:17)], 
  MARGIN = 2 , 
  logreturnsfun
)

# Apply z-score transformation to company-specific variables
# NI and A-MCAP
ScaleD = aggregate(data[,c(5,7)], by = list(data$Company), 
                   simplify = FALSE, FUN = scale)
A = unlist(ScaleD$NI)
A = as.matrix(cbind(A,unlist(ScaleD$`BVE-MCAP`)))
colnames(A) = colnames(data[,c(5,7)])


#deleting false log return#
Datatrans = data.frame(cbind(dataHelp[-1,],LogR, A[-1,]))
Datatrans2 = subset(Datatrans, as.Date("1996-06-30")<Datatrans$Date)
#checking wich row have been deleted
H = anti_join(Datatrans,Datatrans2)
H$Date == "1996-06-28"
rm(H)
# The correct date, for which no log return can be obtained was deleted

# Final adjustments
dataFinal = Datatrans2[,c(1,2,6,3,17, 4, 18, 7,8,9:16,5)] # order data as before
colnames(dataFinal) = colnames(data) # assign previous column names
data = dataFinal 
# remove auxiliary variables / data frames
rm(Datatrans2, Datatrans, LogR, ScaleD, 
   dataHelp, A, dataFinal)

# Generate Market excess return
data$Market = data$Market - data$T.Bill3M
# Remove 3 Month T-Bill Rate
data$T.Bill3M = NULL

# Store transformed dataset for regression analysis and graphical analysis
save(data, file="./Data-Set/For_Marcus_OK_Old_Version.RData")


# === Stationarity Tests for transformed data ===
# (1) Apply panel data unit root test
object <- as.data.frame(split(data[,3:11], data$Company))
PanelUnitRootTest = purtest(object = object, test = "levinlin", 
                            exo = "trend", lags = "AIC", pmax = 5)
sink(file = "./Stationarity-Tests/Stationarity__Return_Panel_Test.txt")
PanelUnitRootTest$statistic
sink()
rm(object, PanelUnitRootTest)
# result: According to this test, the variables in our panel are stationary

# Stationarity Test for common factors- ADF
Sub1 = ComFSep(data)
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
# all values ok - all stationary
write.csv2(testresult, file = "./Stationarity-Tests/Stationarity_CommonFactors_Returns_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "./Stationarity-Tests/Stationarity_Test_Returns_CommonFactors_ADF.txt")
rm(testresult)

# Stationarity Test for common factors- KPSS
testresult2 = apply(Sub1, MARGIN = 2, FUN = stattestFUN2)
testresult2 = data.frame(testresult2)
# Note that H1: non-trend-stationarity of EUR/USD is accepted for EUR/USD at 10%,
# but not at the 5% level
write.csv2(testresult2, file = "./Stationarity-Tests/Stationarity_CommonFactors_Returns_KPSS.csv") # csv table export
print.xtable(xtable(testresult2, auto = TRUE), file = "./Stationarity-Tests/Stationarity_Test_Returns_CommonFactors_KPSS.txt")
# Dataset Sub1 not needed now - discard it
rm(Sub1, testresult2)

# Stationarity Test for company-specific factors as returns - ADF Test
testresult = aggregate(data[,3:7], by = list(data$Company), 
                       FUN = stattestFUN, simplify = FALSE)
testresult = testresult[,-1]
testresult <- do.call("rbind", lapply(testresult, as.data.frame)) 
colnames(testresult) = levels(data$Company) 
testresult = as.data.frame(t(testresult))
write.csv2(testresult, file = "./Stationarity-Tests/Stationarity__Returns_Company_Specific_ADF.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "./Stationarity-Tests/Stationarity_Test_Returns_Company-Specific_ADF.txt")


# Stationarity Test for company-specific factors as returns - KPSS Test
testresult2 = aggregate(data[,3:7], by = list(data$Company), 
                        FUN = stattestFUN2, simplify = FALSE)
testresult2 = testresult2[,-1]
testresult2 <- do.call("rbind", lapply(testresult2, as.data.frame)) 
colnames(testresult2) = levels(data$Company) 
testresult2 = as.data.frame(t(testresult2))
write.csv2(testresult2, file = "./Stationarity-Tests/Stationarity__Returns_Company_Specific_KPSS.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "./Stationarity-Tests/Stationarity_Test_Returns_Company-Specific_KPSS.txt")

rm(list = ls())


# ---------------------------------------------------
# ---------------------------------------------------
# Old Code Sections, not used


#firstdiffun = function(x){
#  n = length(x)
#  firstdif = diff(x, lag=1)
#}






# Add the variable Assets to our dataset in order to obtain
# Net Income over Assets: NI / A 
#CompAssets = read.csv2("./Data-Set/Company_TotAssets.csv", stringsAsFactors = FALSE)
#CompAssets$Date <- as.Date(CompAssets$Date, format = "%d.%m.%Y")
#class(CompAssets$Date)
# If the classes of the Company indicator match - we can continue
#class(data$Company) == class(CompAssets$Company)

# merge the two datasets
#data = merge(data, CompAssets)
#data$Net.Income = data$Net.Income / data$Assets
#data$Assets = NULL
#names(data) = gsub("Net.Income", "NI_by_Assets", names(data))
# remove the dataset "CompAssets" - not necessary anymore
#rm(CompAssets)
# Are all variables there that we need (17 variables)?
#length(colnames(data)) == 17

#first difference
#FirstDiff = apply (
#  X = data[,c(5,7)], 
#  MARGIN = 2 , 
#  firstdiffun
#)
