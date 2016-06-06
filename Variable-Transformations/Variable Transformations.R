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

#

# Define required functions

# Define step size for returns function
iStepSize = 1

# Function for obtaining the returns
# Note that I copied the functions from Christoph Schult
quarterlyreturnsfun = function(x){
  n = length(x)
  # quarterly returns - as Marcus did in Excel
  quartreturn = (x[(iStepSize + 1):n] - x[1:(n - iStepSize)])/x[1:(n - iStepSize)]
}

logreturnsfun = function(x){
  n = length(x)
  logreturn = diff(log(x), iStepSize)
  # AS numeric part added
  }



setwd("C:/Users/Trimme/Documents/Studium Master/Kurse/2016 SoSe/Statistical Programming Languages/Dataset")
data <- read.csv2("Dataset-FINALupdated_absolute.csv", stringsAsFactors=FALSE)


# Convert Date such that R recognizes it as date
class(data$Date)
data$Date <- as.Date(data$Date, format = "%d.%m.%Y")
class(data$Date)

# Generate and store date vector for later purposes
DateVec <- subset(data$Date, data$Company == 1)
#   data$Date[1:79]
class(DateVec)
DateVec <- sort(DateVec)


# First test - went well
# Sub1 = subset(data, data$Company == 1)
# Sub1 = Sub1[order(Sub1$Date),]
# sort the data according to date ascending
# Sub2 = Sub1[,3:11]
# Sub2 = data.frame(apply(Sub2, MARGIN = 2, FUN = logreturnsfun))
# Sub3 = cbind(Sub1[-1,1:2], Sub2)

# Save this as a csv-file - quarterly returns - just as Marcus computed

i = 1
while(i < 10){
  # First test
  Sub1 = subset(data, data$Company == i)
  Sub1 = Sub1[order(Sub1$Date),]
  # sort the data according to date ascending
  Sub2 = Sub1[,3:11]
  Sub2 = data.frame(apply(Sub2, MARGIN = 2, FUN = quarterlyreturnsfun))
  Sub2 = cbind(Sub1[-1,1:2], Sub2)
    if(i == 1){
      Sub3 = Sub2
    }else if(i != 1 & i < 10){
      Sub3 = rbind(Sub3, Sub2 )
    }else{
      # Do nothing
    }
    i = i + 1
}


# Using log-returns I face the following "simple" problem
# Net Income, for instance, can be negalive
# Yet log(-1000) returns NA 
# Do you guys have a first-glace idea how to tackle that?




# Export the resulting transformation
write.csv2(Sub3, file = "Z_Transformed_variables_Quarterly_returns_as_Marcus.csv")
