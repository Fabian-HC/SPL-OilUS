# remove variables
rm(list = ls())

# reset graphics
graphics.off()


libraries = c("psych", "xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
# Package loading done - variable can be deleted
rm(libraries)

# For my customized summary later on
SumCustom <- function(x){
  r1 = mean(x)
  r2 = sd(x)
  r3 = quantile(x, probs = c(0 ,0.25, 0.5, 0.75, 1), na.rm = TRUE)
  r4 = skew(x, na.rm = TRUE, type = 3)
  r5 = kurtosi(x, na.rm = TRUE, type = 3)
  SumMat = (matrix(c(r1, r2, r3, r4, r5), ncol = 9))
  colnames(SumMat) = c("mean", "sd", "min", "q(25)", "q(50)", "q(75)", "max", "skew", "kurtosis")
  return(SumMat)
  
}





# Set working directory
# Set it equal to where your directory is
setwd("C:/Users/Trimme/Documents/GitHub/R_Project/SPL-OilUS")
# setwd("C:/Users/Trimme/Documents/Studium Master/Kurse/2016 SoSe/Statistical Programming Languages/Dataset", st)
data = read.csv2("~/GitHub/R_Project/SPL-OilUS/Data-Set/Dataset-FINALupdated_absolute.csv", stringsAsFactors = FALSE)

# associate names to compagnie 
# WHEN ACTIVATING THIS; WE ALSO HAVE TO CHANGE THE SUBSET OPTIONS!

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




# Summary statistics for common factors
Sub1 = subset(data, data$Company == 1)
SumCommonF = data.frame(apply(Sub1[,8:11], MARGIN = 2, FUN = SumCustom))
rownames(SumCommonF) = c("mean", "sd", "min", "q(25)", "q(50)", "q(75)", "max", "skew", "kurtosis")
# Export as CSV
write.csv2(SumCommonF, file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Common_Factors_Absolute.csv")
# Export as TexFile
print.xtable(xtable(SumCommonF), file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Common_Factors_Absolute.txt")


# Summary statistics of company-specific variables
SumSpecF = describeBy(data[,2:7], group = "Company", mat = TRUE, digits = 2)
class(SumSpecF)
SumSpecF = SumSpecF[-c(1:9),-c(4,8,9,12,15)]
SumSpecF = SumSpecF[order(SumSpecF$group1, SumSpecF$vars),]
SumSpecF = SumSpecF[,-(1:3)]
write.csv2(SumSpecF, file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Specific_Factors_Absolute.csv")
# Export as TexFile
print.xtable(xtable(SumSpecF), file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Specific_Factors_Absolute.txt")


rm(data, Sub1, SumCommonF, SumSpecF)
#-------------------------------------------------------------
#-------------------------------------------------------------
# Summary statistics for return verion of variables
load("~/GitHub/R_Project/SPL-OilUS/Data-Set/TransformedDate.RData", verbose = TRUE)

# Summary statistics for common factors
Sub1 = subset(dataFinal, dataFinal$Company == 1)
SumCommonF = data.frame(apply(Sub1[,8:11], MARGIN = 2, FUN = SumCustom))
rownames(SumCommonF) = c("mean", "sd", "min", "q(25)", "q(50)", "q(75)", "max", "skew", "kurtosis")
# Export as CSV
write.csv2(SumCommonF, file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Common_Factors_returns.csv")
# Export as TexFile
print.xtable(xtable(SumCommonF), file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Common_Factors_returns.txt")

# Summary statistics of company-specific variables
SumSpecF = describeBy(dataFinal[,2:7], group = "Company", mat = TRUE, digits = 2)
class(SumSpecF)
SumSpecF = SumSpecF[-c(1:9),-c(4,8,9,12,15)]
SumSpecF = SumSpecF[order(SumSpecF$group1, SumSpecF$vars),]
SumSpecF = SumSpecF[,-(1:3)]
write.csv2(SumSpecF, file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Specific_Factors_returns.csv")
# Export as TexFile
print.xtable(xtable(SumSpecF), file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Specific_Factors_returns.txt")




# -------------------------------------------------------
# -------------------------------------------------------

# OLD CODE

# An attempt to resort to one loop less
# Define company-specific variable set
colnames(data[, 2:7])
SumStatList = describeBy(data[, 2:7], group = data$Company, mat = FALSE)

i = 1
while(i < 10){
  Filename = paste("~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/Specific_Factors_Absolute_Company_", i, "SumStat.csv", sep = "")
  TexExport = paste("~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/Company_", i, "SumStat.txt", sep = "")
  # DataWrite = paste("SplitSumTable$", i, sep = "")
  SumTable = data.frame(SumStatList[i])
  SumTable = SumTable[-1,-c(1,2,6,7,10,13)]
  colnames(SumTable) = c("mean", "min", "median", "min", "max", "skew", "kurtosis") 
  SumTable = round(SumTable, digits = 2)
  write.csv2(SumTable, file = Filename) # csv table export
  print.xtable(xtable(SumTable, auto = TRUE), file = TexExport)
  i = i + 1 
}

# Still substitute vars by variable names and then we would be fine
# Subsetting the tables is also a problem, but that could be overcome!

# install.packages("plyr")
# library(plyr)
# ddply(SumTable, ~group1)

# Hier morgen wie folgt weiter
# 1: per (l/t/m)-apply nach Company auftrennen und als CSV exportieren



# CODE NOT USED ANYMORE

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

# OLD CODE NOT TO USE ANYMORE
# Done

# Summary of Common Factors
# Note that it is enough to pick the common factors from the 1st
# Company, because they are identical for all of them
CommonF <- data.frame(subset(data[, 8:10], data$Company == 1))
SumMat <- data.frame(describe(CommonF))
# SumMat <- data.frame(SumMat) # Convert into a data frame
# SumMat - Uncomment to take a look at it in R
SumMat <- round(SumMat, digits = 2)
SumMat <- data.frame(SumMat)
# I am interested in keeping: mean, sd, median, min, max, skey, kurtosis
StatInterest =  c("mean", "sd", "median", "min", "max", "skew", "kurtosis")
SumMat = SumMat[StatInterest]
SumMat



# Approach SUMMARY SPECIFIC FACTORS USING AGGREGATE
# Columns: variables
# Rows: moments, stat of interes
T = (aggregate(data[,3:7], by = list(data$Company), FUN = SumCustom, simplify = FALSE))
T = as.list(T)
(sapply(T, unlist, simplify = FALSE))

T$Stock

T1 = mapply(unlist, T, MoreArgs = NULL, SIMPLIFY = FALSE)
T1 = as.matrix(T1)


T1 = T = matrix(unlist(T), ncol = 9, byrow = TRUE)
unlist(T)

T[1,2]

L = split(data[3:7], data$Company)

apply(split(data[3:7], data$Company), MARGIN = 2, FUN = SumCustom)
