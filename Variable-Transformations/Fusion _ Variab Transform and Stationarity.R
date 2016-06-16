# remove variables
rm(list = ls())

# reset graphics
graphics.off()

# Install packages if not installed
libraries = c("plyr","dplyr","data.table", "tseries", "xtable")
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

# Perform stationarity Tests on the absolute level

stattestFUN = function(x){
  test = adf.test(x, alternative = "stationary")
  testresult = test$p.value
  testresult = round(testresult, digits = 2)
  #ReturnResult = if(testresult < 0.1){
  #  ReturnResult = "stationary"
  #}else{
  #  ReturnResult ="not_stationary"
  #}
}

# Stationarity Test for common factors
Sub1 = subset(data, data$Company == 1)
Sub1 = Sub1[,8:11]
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_CommonFactors_Absolute.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_CommonFactors_Absolute.txt")
# Dataset Sub1 not needed now - discard it
rm(Sub1)


# Stationarity Test for company-specific factors
testresult = aggregate(data[,3:7], by = list(data$Company), FUN = stattestFUN, simplify = FALSE)
testresult = data.frame(testresult)
write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity__Absolute_Company.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_Absolute_Company-Specific.txt")




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

# remove auxiliary variables we do not need anymore
rm(FirstDiff, LogR, Datatrans2, Datatrans)

# Stationarity Test for common factors
Sub1 = subset(dataFinal, dataFinal$Company == 1)
Sub1 = Sub1[,8:11]
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
# all values ok - all stationary
# write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_CommonFactors_Returns.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_Returns_CommonFactors.txt")
# Dataset Sub1 not needed now - discard it
rm(Sub1, testresult)

# Stationarity Test for company-specific factors
testresult = aggregate(dataFinal[,3:7], by = list(dataFinal$Company), FUN = stattestFUN, simplify = FALSE)
testresult = data.frame(testresult)
# write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity__Returns_Company_Cut2012.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_Returns_Company-Specific.txt")




# Save the transformed dataset (if you save it as follow Date remanes Date)
# Antton's save place
# Note that panel data transformation has not been applied to this data set so far
# save(dataFinal, file="TransformedData.RData")
save(dataFinal, file="~/GitHub/R_Project/SPL-OilUS/Data-Set/TransformedDate.RData")
rm(data, Datatrans2, Datatrans)
load("~/GitHub/R_Project/SPL-OilUS/Data-Set/TransformedDate.RData", verbose = TRUE)
#write.csv2(dataFinal, file = "Z_Transformed_variables_Quarterly_returns_as_Marcus.csv")


# Set a cutoff at 2012

dataFinal = subset(dataFinal, as.Date("2013-03-28")>dataFinal$Date)

# Stationarity Test for common factors
Sub1 = subset(dataFinal, dataFinal$Company == 1)
Sub1 = Sub1[,8:11]
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
# all values ok - all stationary
#write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_CommonFactors_Absolute.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_CommonFactors_Returns_Cut2012.txt")
# Dataset Sub1 not needed now - discard it
rm(Sub1, testresult)

# Stationarity Test for company-specific factors
testresult = aggregate(dataFinal[,3:7], by = list(dataFinal$Company), FUN = stattestFUN, simplify = FALSE)
testresult = data.frame(testresult[,-1])
class(testresult[,1])
# Most stationarity problems drop out when we exclude the years 2013 - 2015
# write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity__Absolute_Company.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_Returns_Company-Specific_Cut2012..txt")




# associate names to compagnie 
#data$Company = as.character(data$Company)
#data$Company[data$Company== "1"] ="Exxon Mobil Corp"
#data$Company[data$Company== "2"] ="Apache Corp"
#data$Company[data$Company== "3"] ="CenterPoint Energy Inc"
#data$Company[data$Company== "4"] ="Chevron Corp"
#data$Company[data$Company== "5"] ="Hess Corp"
#data$Company[data$Company== "6"] ="Murphy Oil Corporation"
#data$Company[data$Company== "7"] ="Occidental Petroleum Corp"
#data$Company[data$Company== "8"] ="PG&E Corp"
#data$Company[data$Company== "9"] ="Williams Cos Inc"
