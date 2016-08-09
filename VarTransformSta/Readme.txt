Name of Quantlet: VarTransStation

Published in: Statistical 'Programming Languages Seminar 2016; Ladislaus von Bortkiewicz Chair of Statistics, Humboldt University of Berlin'

Description: '

This quantlet reads in the previously compiled dataset obtained from Bloomberg (or an alternative source providing the same financial
data). In a first step, the corresponding company names as well as more suitable shorthand variable names are added 
and the date variable is formatted adequately. Note that the panel data is NOT transformed into a pdata frame yet. 
This occurs only in the econometric analysis quantlets.


Before the data is transformed according to Bianconi and Yoshino (2014) , let's check whether the panel data set is stationary
according to the purtest() contained in the plm package. Since one can suspect nonstationarity 
to be more relevant for some variables (and their respective transformations), 
it is worthwhile to carry out the ADF-test and the KPSS-test (both of the plm package as well) 
per company and variable. All tests point to that the variables in our panel are nonstationarity. 

Keeping the results of this quantlet in mind, one can then proceed to the explorative data analysis quantlet, 
EDA_PanDat or the regression analyses quantlets (Quantlet 3_PanelDataAnalysis, Quantlet 4_TimeSeriesAnalysis). 

Transformations of variables applied
(1) Taking logs: Debt over Equity (D-MCAP), Assets over Market Cap (A-MCAP)
(2) Deriving log returns: Stock Prices, Exchange rates, Oil Price, Gas Price, 
(3) Variable standardization by company: Net Income (NI) and Book Value of Equity over its market value (BVE-MCAP)

Note that the fist and the last transformation, by just demeaning and scaling the variables are suspected to not remove the potential nonstationarity of these variables.

Please note that, according to Phillips and Moon (2000), nonstationarity in panel data 
might affect panel data regression procedures less if the panel dimensions (time and individuals) grow larger. 
Yet since the sample of this analysis is only (N=7 (9) and T = 80), nonstationary variables 
might still adversely affect the regression results.'

Keywords: 'transformation , panel-analysis, stationary, stock-price, data adjustment'

Author: Tim Halbmann

Datafile: 'Dataset-FINALupdated_absolute_2.csv'

See Also: 'PanelDataAnalysis, SubsampleAnalysis, TimeSeriesAnalysis, EDA_PanDat'

Quantlet Code:

# === Clear the screen ===
cat("\014")

# Tested With: 
# R version 3.2.4 Revised (2016-03-16 r70336) -- "Very Secure Dishes"
# Copyright (C) 2016 The R Foundation for Statistical Computing
# Platform: i386-w64-mingw32/i386 (32-bit)


# === Clearing the Environment ===
# remove variables
rm(list = ls(all = TRUE))

# reset graphics
graphics.off()


# === Set adequate working directory ===
setwd("~/GitHub/Test/SPL-OilUS_2/VarTransformSta1")


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

# Function for seperating the common factor developments apart from the others
ComFSep = function(x){
    if(class(x) == "data.frame"){
        Sub1 = subset(x, x$Company == levels(x$Company)[1])
        Sub1 = Sub1[,c("Oil", "Gas", "Market", "EURUSD")]
        return(Sub1)
    }else{
        print("Data must be passed as data frame or a subsection of it")
  }
}



# Functions for variable transformations later on
# Obtain log returns for variables of interest
logreturnsfun = function(x){
    n         = length(x)
    logreturn = diff(log(x), lag=1)
  # AS numeric part added
}

# Obtain z-scores for other variables of interst
scalefun = function(x){
    ScaleVar = scale(x)
}

# Stationarity Test - ADF TEst
stattestFUN = function(x){
    test       = adf.test(x, alternative = "stationary")
    testresult = test$p.value
    testresult = round(testresult, digits = 2)
    if(testresult < 0.1){
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
    if(test < 0.099){
        ReturnResult = "non (trend) stationary"
    }else{
        ReturnResult ="(trend) stationary"
    }
}

CommonFStatTest = function(data){
    Sub1 = ComFSep(data)
    # ADF Test
    ADFComTest      = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
    ADFComTest      = data.frame(ADFComTest)
    # KPSS Test
    KPSSComTest     = apply(Sub1, MARGIN = 2, FUN = stattestFUN2)
    KPSSComTest     = data.frame(KPSSComTest)
    # Reunify findings in one table
    TestComFSummary = cbind(ADFComTest, KPSSComTest)
    colnames(TestComFSummary) = c("ADF-Test", "KPSS-Test")
    return(TestComFSummary)
}

ADFEntSpecTestFun = function(x){
    ADFTest           = aggregate(x, by = list(data$Company), 
                                  FUN = stattestFUN, simplify = FALSE)
  
    ADFTest           = ADFTest[,-1]
    ADFTest           = do.call("rbind", lapply(ADFTest, as.data.frame))
    colnames(ADFTest) = levels(data$Company)
    ADFTest           = as.data.frame(t(ADFTest))
  return(ADFTest)
}

KPSSEntSpecTestFun = function(x){
    KPSSTest           = aggregate(x, by = list(data$Company), 
                                   FUN = stattestFUN2, simplify = FALSE)
    KPSSTest           = KPSSTest[,-1]
    KPSSTest           = do.call("rbind", lapply(KPSSTest, as.data.frame))
    colnames(KPSSTest) = levels(data$Company)
    KPSSTest           = as.data.frame(t(KPSSTest))
    return(KPSSTest)
}



# ======= Initial data modifications =====
# loading data
data = read.csv2("./Dataset-FINALupdated_absolute_2.csv", 
                 stringsAsFactors = FALSE)

# Convert Date such that R recognizes it as date
class(data$Date)
data$Date = as.Date(data$Date, format = "%d.%m.%Y")
class(data$Date)

# Order data adequately for subsequent transformations
data = data[order(data$Company, data$Date),]


# Set shorter ColumnNames for subsequent analysis
# Note that non-ambiguous variable names have to be assinged (no /, Ä, Ö, ...)
colnames(data) = c("Date", "Company", "Stock", "A.MCAP", 
                   "BVE.MCAP", "D.MCAP", "NI", 
                   colnames(data[8:ncol(data)])) 

# Attach companyNames to the data set
# Company variable from integer to factor
data$Company = as.factor(data$Company) 
levels(data$Company) = c("Exxon_Mobil", "Apache", 
                         "CPEnergy", "Chevron", "Hess_Corp", "Murphy_Oil", 
                         "Occidental_Petroleum", "PG&E_Corp", "Williams")

# Save the dataset - used by Quantlet2 _ EDA
save(data, file="./InitialData_Panel.RData") 


# === Apply Variable Transformations ==
# Apply log - transformation to variables: A.MCAP, D.MCAP, 
dataHelp           = cbind(data[,1:2], log(data[, c(4,6)]), data[,c(18)])
colnames(dataHelp) = colnames(data[,c(1:2, 4,6, 18)])


# Apply log-return transformation to predefinded variable set 
# by variable and company
LogR = aggregate(data[,c(3, 8:17)], by = list(data$Company), 
                 simplify = FALSE, FUN = logreturnsfun)
A   = do.call("cbind", lapply(LogR, unlist))
A   = A[,-1] # remove irrelevant variable 'group 1'
rm(LogR)

# Apply z-score transformation to predefinded variable set 
# by variable and company
ScaleD = aggregate(data[,c(5,7)], by = list(data$Company), 
                   simplify = FALSE, FUN = scale)
D      = do.call("cbind", lapply(ScaleD, unlist))
D      = D[,-1] # remove irrelevant variable 'group 1'
rm(ScaleD) # remove auxiliary storage variable 'ScaleD'

# Merge transformations applied to a new data set
Datatrans = data.frame(cbind(dataHelp[-1,],D[-1,]))
Datatrans = subset(Datatrans, as.Date("1996-06-30")<Datatrans$Date)
Datatrans = data.frame(cbind(Datatrans, A))
Datatrans = Datatrans[,colnames(data)]
rm(A, D, dataHelp) # remove auxiliary variables composing Datatrans


# Generate Market excess return
Datatrans$Market   = Datatrans$Market - Datatrans$T.Bill3M
Datatrans$T.Bill3M = NULL # Remove 3 Month T-Bill Rate
# Since Market Excess Return is a common factor
Datatrans$Market   = Datatrans$Market[1:78]

data = Datatrans

# Store transformed dataset for regression analysis and graphical analysis
save(data, file="./RegressionBase2.RData")
rm(data, Datatrans)



# === Stationarity Tests for Variables before transformation(s) ===
load(file = "./InitialData_Panel.RData")

# Stationarity Test for common factors - ADF and KPSS Test
CommonFTest = CommonFStatTest(data)
write.csv2(CommonFTest, file = "./Stationarity_CommonFactors_Absolute.csv") # csv table export
print.xtable(xtable(CommonFTest, auto = TRUE), 
             file = "./Stationarity_Test_CommonFactors_Absolute.txt")
rm(CommonFTest)

# Stationarity Test for company-specific factors - ADF Test
ADFTest = ADFEntSpecTestFun(data[,3:7])
write.csv2(ADFTest, file = "./Stationarity_Company_Specific_ADF__Absolute.csv") # csv table export
print.xtable(xtable(ADFTest, auto = TRUE), file = "./Stationarity_Test_Company-Specific_ADF_Absolute.txt")
rm(ADFTest) # remove auxiliary variable


# Statioinarity Test for company-specific factors - KPSS Test
KPSSTest = KPSSEntSpecTestFun(data[,3:7])
write.csv2(KPSSTest, file = "./Stationarity_Company_Specific_KPSS_Absolute.csv") # csv table export
print.xtable(xtable(KPSSTest, auto = TRUE), file = "./Stationarity_Test_Company-Specific_KPSS_Absolute.txt")
rm(KPSSTest) # remove auxiliary variable

# Perform a panel unit root test
object            = as.data.frame(split(data[,3:18], data$Company))
PanelUnitRootTest = purtest(object = object, test = "levinlin", 
                            exo = "trend", lags = "AIC", pmax = 5)
sink(file = "./Stationarity_Panel_Test__Absolute.txt")
PanelUnitRootTest$statistic
sink()

rm(object, PanelUnitRootTest, data)



# === Stationarity Tests for transformed data ===
# load transformed data
load(file = "./RegressionBase2.RData", verbose = FALSE)

# (1) Apply panel data unit root test
object            = as.data.frame(split(data[,3:11], data$Company))
PanelUnitRootTest = purtest(object = object, test = "levinlin", 
                            exo = "trend", lags = "AIC", pmax = 5)
sink(file = "./Stationarity__Return_Panel_Test_Transformed.txt")
PanelUnitRootTest$statistic
sink()
rm(object, PanelUnitRootTest)

# Stationarity Test for common factors- ADF and KPSS
CommonFTest = CommonFStatTest(data)
# all values ok - all stationary
write.csv2(CommonFTest, file = "./Stationarity_CommonFactors_Transformed.csv") # csv table export
print.xtable(xtable(CommonFTest, auto = TRUE), file = "./Stationarity_Test_CommonFactors_Transformed.txt")
rm(CommonFTest) # remove auxiliary variable

# Stationarity Test for company-specific factors as returns - ADF Test
ADFTest = ADFEntSpecTestFun(data[,3:7])
write.csv2(ADFTest, file = "./Stationarity_Company_Specific_ADF_Transformed.csv") # csv table export
print.xtable(xtable(ADFTest, auto = TRUE), 
             file = "./Stationarity_Test_Returns_Company-Specific_ADF_Transformed.txt")
rm(ADFTest)

# Stationarity Test for company-specific factors as returns - KPSS Test
KPSSTest = KPSSEntSpecTestFun(data[,3:7])
write.csv2(KPSSTest, file = "./Stationarity_Company_Specific_KPSS_Transformed.csv") # csv table export
print.xtable(xtable(KPSSTest, auto = TRUE), file = "./Stationarity_Test_Returns_Company-Specific_KPSS_Transformed.txt")

rm(list = ls(all = TRUE))# Since Market Excess Return is a common factor

