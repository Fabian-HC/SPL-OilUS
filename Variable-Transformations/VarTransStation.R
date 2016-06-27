# asdf
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
