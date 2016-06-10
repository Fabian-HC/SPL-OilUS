# Test Test Test

# Bla Bla


#adsfjahsf

# asjdfhasjdf
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
# iStepSize = 1




zScoreFun = function(x){
  n = length(x)
  result = scale(x, center = TRUE, scale = TRUE)
  # result = ((x - mean(x)) / sd(x)) 
}

#quarterlyreturnsfun = function(x){
 # n = length(x)
  # quarterly returns - as Marcus did in Excel
 # quartreturn = (x[(iStepSize + 1):n] - x[1:(n - iStepSize)])/x[1:(n - iStepSize)]
#}

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


LogRetSection = data[,c(1:2, 8:11, 3, 4, 6)]

# generate log-return section
iStepSize = 1
z = ncol(LogRetSection)
i = 1
while(i < 10){
  # First test
  Sub1 = subset(LogRetSection, LogRetSection$Company == i)
  Sub1 = Sub1[order(as.Date(Sub1$Date, format = "%Y/%m/%d")),]
  Sub2 = Sub1[,3:z]
  Sub2 = data.frame(apply(Sub2, MARGIN = 2, FUN = logreturnsfun))
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
DataLogReturnSection = Sub3
# Remove the date 1996-06-28
# M.r[, -which(colnames(M.r) == "w")] # If we have assigned names to columns and rows,
# DataLogReturnSection = DataLogReturnSection[-which()]
rm(LogRetSection)

ZScoreSection = data[,c(1:2,5,7)]
dim(ZScoreSection)

z = ncol(ZScoreSection)
i = 1
while(i < 10){
  # First test
  Sub1 = subset(ZScoreSection, ZScoreSection$Company == i)
  Sub1 = Sub1[order(as.Date(Sub1$Date, format = "%Y/%m/%d")),]
  # Sub1 = Sub1[-nrow(Sub1),]
  Sub2 = Sub1[,3:z]
  Sub2 = data.frame(apply(Sub2, MARGIN = 2, FUN = zScoreFun))
  Sub2 = cbind(Sub1[-1,1:2], Sub2[-1,])
  if(i == 1){
    Sub3 = Sub2
  }else if(i != 1 & i < 10){
    Sub3 = rbind(Sub3, Sub2 )
  }else{
    # Do nothing
  }
  i = i + 1
}
DataZScoresSection = Sub3
rm(Sub1, Sub2, Sub3, i, iStepSize, z)
# Merge the two data sections
dataTrans = cbind(DataLogReturnSection, DataZScoresSection[,3:4])
dataTrans = dataTrans[,-1]


# Save the transformed dataset
write.csv2(dataTrans, file = "Z_Transformed_variables_Quarterly_returns_as_Marcus.csv")








# Code NOT USED
VarNameSet = variable.names(data)
LogRetVar = colnames(data[,c(8:11, 3, 4, 6)])
ZScoreVar = colnames(data[,c(5, 7)])
RemVar = colnames(data[,c(1,2)])

Sub1 = subset(data, data$Company == 1)
Sub1 = Sub1[order(Sub1$Date),]
Sub2 = Sub1[,3:4]
result = "NA"
VarName = colnames(Sub2)
any(VarName == LogRetVar)

Test = transformFUN(Sub1[,3])
# TransformFUN wird nicht richtig verstanden
Sub2 = data.frame(apply(Sub2, MARGIN = 2, FUN = transformFUN))

Sub2 = cbind(Sub1[-1,1:2], Sub2)

i = 1
while(i < 2){
  # First test
  Sub1 = subset(data, data$Company == i)
  Sub1 = Sub1[order(Sub1$Date),]
  # sort the data according to date ascending
  Sub2 = Sub1[,3:11]
  Sub2 = data.frame(apply(Sub2, MARGIN = 2, FUN = transformFUN))
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
# Es gibt einen Umweg ?ber die Funktion by oder 'aggregate'. Das
# hat jedoch bei mir noch nicht geklappt


# Using log-returns I face the following "simple" problem
# Net Income, for instance, can be negalive
# Yet log(-1000) returns NA 
# Do you guys have a first-glace idea how to tackle that?




# Export the resulting transformation
write.csv2(Sub3, file = "Z_Transformed_variables_Quarterly_returns_as_Marcus.csv")



# Function for obtaining the returns
# Note that I copied the functions from Christoph Schult
transformFUN = function(x){
  VarName = colnames(x)
  if(any(VarName == LogRetVar)){
    n = length(x)
    result = diff(log(x), iStepSize)
  }else if(any(VarName == ZScoreVar)){
    n = length(x)
    result = scale(x)
  }else{
    # nothing  
  }
  return(result)
  # End of the function
}