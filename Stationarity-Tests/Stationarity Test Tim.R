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
  ReturnResult = if(testresult < 0.05){
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

Sub1 = subset(data, data$Company == 3)
apply(Sub1[,3:11], MARGIN = 2, FUN = stattestFUN)

adf.test(Sub1$Stock, alternative = "explosive")

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

testresult = data.frame(testresult)



# There must be some way using the by-function
# Or other ways, yet I did not find the key :(
by(data = data, INDICES = list("Company"), FUN = stattestFUN)

lapplyBy(~Company, data = data, FUN = stattestFUN, keep.groupid = TRUE)

tapply(X = data[,c(2,8:11)], INDEX = data$Company, FUN = stattestFUN)

length(data[,c(1,8:11)])
length(data[,2])

lapplyBy(~Company, data = data[,-c(1,2)], FUN = mean)

??adf.test

