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

# For my customized summary later on
SumCustom <- function(x){
  r1 = mean(x)
  r2 = median(x)
  r3 = sd(x)
  r4 = min(x)
  r5 = max(x)
  
  return(matrix(c(r1, r2, r3, r4, r5), ncol = 5))
  
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
# DateVec

# Convert data into panel data
data <- pdata.frame(data, index = c("Company", "Date"), drop.index = F, row.names = T) 

# Has R read in the panel Data set with Date as date variable?
class(data$Date)
# If not, let's change that
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
class(data$Date)
# Done

# Summary of Common Factors
# Set up a 'customized' Summary Table for the common factors
# Note that it is enough to pick the common factors from the 1st
# Company, because they are identical for all of them
CommonF <- data.frame(subset(data[, 8:10], data$Company == 1))
SumMat <- describe(CommonF)
SumMat <- data.frame(SumComFact) # Convert into a data frame
# SumMat - Uncomment to take a look at it in R
SumMat <- round(SumMat, digits = 2)
SumMat <- data.frame(SumMat)
SumMat
write.csv2(SumMat, file = "Z _ Summary_Common_Factors_Absolute.csv")


# Summary of specific factors for respective firms
# First loop to select the different enterprises
i = 1
while(i < 10){
  
  Sub1 <- subset(data, data$Company == i)
  
  SumMat1 <- matrix(NA, ncol = 5, nrow = 5)
  StatInterest <- c("mean", "median", "sd", "min", "max")
  colnames(SumMat1) <- StatInterest
  rownames(SumMat1) <- variable.names(Sub1[,3:7])
  
  # Loop in the Loop to retrieve information of variables of interes
  j = 3
  while(j < 8){
    SumMat1[(j - 2),] <- SumCustom(Sub1[,j])
    j = j + 1
  }
  SumMat1 <- round(SumMat1, digits = 4)
  SumMat1 <- data.frame(SumMat1)
  Filename <- paste("Specific_Factors_Company_", i, "_Absolute.csv", sep = "" )
  write.csv2(SumMat1, file = Filename)
  rm(Sub1)
  rm(SumMat1)
  i = i + 1
}



# An attempt no to resort to a loop
# Define company-specific variable set
colnames(data[, 2:7])
# That is the enterprise-specific information I would like to sumamrize
specific <- data[, 2:7]
# describeBy(specific, group = specific$Company) # result can be presented better
EntSummaryMat <- describeBy(specific, group = specific$Company, mat = TRUE)
SumTable <- data.frame(EntSummaryMat)
SumTable <- orderBy(~group1, data = SumTable)
SumTable <- data.frame(SumTable[,2:15]) # remove more elegantly later
SumTable = SumTable[, -which(colnames(SumTable) == "n")]
SumTable
# Still substitute vars by variable names and then we would be fine
# Subsetting the tables is also a problem, but that could be overcome!

# install.packages("plyr")
# library(plyr)
# ddply(SumTable, ~group1)

# Hier morgen wie folgt weiter
# 1: per (l/t/m)-apply nach Company auftrennen und als CSV exportieren
