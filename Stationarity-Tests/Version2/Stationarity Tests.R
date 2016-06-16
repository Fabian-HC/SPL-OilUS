# Load packages

library(Cairo)

# Package for sumamrizing data
# install.packages("Hmisc")
library(Hmisc)
# for this command: describe(mydata)

# Library for sumamrizing data 2
# install.packages("psych")
library(psych)
# for this command: describe.by(mydata, group,...)

# Working with Doby package for summary Statistics
# install.packages("doBy")
library(doBy)
#summaryBy(mpg + wt ~ cyl + vs, data = mtcars, 
#          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

# Importing files of other formats
library(foreign)

# Panel data analysis package
library(plm) 

library(car)

library(lmtest)   

library(sandwich)

library(ggplot2)

library(grid) # needed for plotting later
library(lattice)
library("reshape2")
library(xtable)
library(tseries)

stattestFUN = function(x){
  test = adf.test(x, alternative = "stationary")
  testresult = test$p.value
  ReturnResult = if(testresult < 0.1){
                  ReturnResult = "stationary"
                 }else{
                  ReturnResult ="not_stationary"
                }
}

# Set working directory
# Set it equal to where your directory is!!!!!!
setwd("C:/Users/Trimme/Documents/GitHub/R_Project/SPL-OilUS")
# setwd("C:/Users/Trimme/Documents/Studium Master/Kurse/2016 SoSe/Statistical Programming Languages/Dataset", st)
data = read.csv2("~/GitHub/R_Project/SPL-OilUS/Data-Set/Dataset-FINALupdated_absolute.csv", stringsAsFactors = FALSE)


# Convert Date such that R recognizes it as date

class(data$Date)
data$Date <- as.Date(data$Date, format = "%d.%m.%Y")
class(data$Date)


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


# Generate and store date vector for later purposes
DateVec <- subset(data$Date, data$Company == 1)
#   data$Date[1:79]
class(DateVec)
DateVec <- sort(DateVec)
# DateVec

# Convert data into panel data
data <- pdata.frame(data, index = c("Company", "Date"), drop.index = F, row.names = T) 

# Has R read in the panel Data set with Date as date variable?
class(data$Date)
# If not, let's change that
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
class(data$Date)
# Done

# Stationarity Test for common factors
Sub1 = subset(data, data$Company == 1)
Sub1 = Sub1[,8:11]
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_CommonFactors_Absolute.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_CommonFactors_Absolute.txt")

# Stationarity Test for company-specific factors
testresult = aggregate(data[,3:7], by = list(data$Company), FUN = stattestFUN, simplify = FALSE)
testresult = data.frame(testresult)
# write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity__Absolute_Company.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_Absolute_Company-Specific..txt")


rm(data, Sub1, testresult)
# Then let's test our transformed variable set
# setwd("C:/Users/Trimme/Documents/Studium Master/Kurse/2016 SoSe/Statistical Programming Languages/Dataset", st)
data = read.csv2("~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Z_Transformed_variables_Quarterly_returns_as_Marcus.csv", stringsAsFactors = FALSE)


# Convert Date such that R recognizes it as date
class(data$Date)
data$Date <- as.Date(data$Date, format = "%d.%m.%Y")
class(data$Date)


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

# Stationarity Test for common factors
Sub1 = subset(data, data$Company == 1)
Sub1 = Sub1[,3:6]
testresult = apply(Sub1, MARGIN = 2, FUN = stattestFUN)
testresult = data.frame(testresult)
write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_CommonFactors_Returns.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_CommonFactors_Returns.txt")


# Stationarity Test for enterprise-specific factors by enterpirse
testresult = aggregate(data[,7:11], by = list(data$Company), FUN = stattestFUN, simplify = FALSE)
testresult = data.frame(testresult[,-1])
class(testresult)
# write.csv2(testresult, file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity__Absolute_Company.csv") # csv table export
print.xtable(xtable(testresult, auto = TRUE), file = "~/GitHub/R_Project/SPL-OilUS/Variable-Transformations/Stationarity_Test_Returns_Company-Specific..txt")






# OLD CODE FOR TESTING STATIONARITY OF COMPANY SPECIFIC VARIABLES
i = 1
while(i < 10){
  Sub1 = subset(data, data$Company == i)
  helpList = apply(Sub1[,3:11], MARGIN = 2, FUN = stattestFUN)
  if(i == 1){
    testresult = helpList }
  else{
    testresult = mapply(c, testresult, helpList,  SIMPLIFY = FALSE)
  }
  i = i + 1
}
