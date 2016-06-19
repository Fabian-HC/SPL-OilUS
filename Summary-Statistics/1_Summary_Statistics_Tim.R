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

# Set working directory
# Set it equal to where your directory is
setwd("C:/Users/Trimme/Documents/GitHub/R_Project/SPL-OilUS")
# setwd("C:/Users/Trimme/Documents/Studium Master/Kurse/2016 SoSe/Statistical Programming Languages/Dataset", st)
load(file="~/GitHub/R_Project/SPL-OilUS/Data-Set/InitialData_Date_OK.RData", verbose = TRUE)


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
SumCommonF = describe(Sub1[,8:11], skew = TRUE, trim = 0)
SumCommonF = round(SumCommonF[,-c(1,2,6,7,10,13)], digits = 2)
# Export as CSV
write.csv2(SumCommonF, file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Common_Factors_Absolute.csv")
# Export as TexFile
print.xtable(xtable(SumCommonF), file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Common_Factors_Absolute.txt")


# Summary statistics of company-specific variables
SumSpecF = describeBy(data[,2:7], group = "Company", mat = TRUE, digits = 2, trim = 0)
SumSpecF = SumSpecF[-c(1:9),-c(4,8,9,12,15)]
SumSpecF = SumSpecF[order(SumSpecF$group1, SumSpecF$vars),]
SumSpecF = SumSpecF[,-(1:3)]
SumSpecF = SumSpecF[,c(1,3,2,4:7)]
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
SumCommonF = describe(Sub1[,8:11], trim = 0)
SumCommonF = SumCommonF[,-c(1,2,6,7,10,13)]
SumCommonF = SumCommonF[,c(1,3,2,4:7)]
SumCommonF[,c(1,2)] = round(SumCommonF[,c(1,2)], digits = 4)
SumCommonF[,-c(1,2)] = round(SumCommonF[,-c(1,2)], digits = 2)
# Export as CSV
write.csv2(SumCommonF, file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Common_Factors_returns.csv")
# Export as TexFile
print.xtable(xtable(SumCommonF), file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Common_Factors_returns.txt")

# Summary statistics of company-specific variables
SumSpecF = describeBy(dataFinal[,2:7], group = "Company", mat = TRUE, digits = 4, trim = 0)
SumSpecF = SumSpecF[-c(1:9),-c(4,8,9,12,15)]
SumSpecF = SumSpecF[order(SumSpecF$group1, SumSpecF$vars),]
SumSpecF = SumSpecF[,-(1:3)]
SumSpecF = SumSpecF[,c(1,3,2,4:7)]
SumSpecF[,4:7] = round(SumSpecF[,4:7], digits = 2)
SumSpecF[seq(5,45,5),1:5] = round(SumSpecF[seq(5,45,5),1:5], digits = 0) # R does not get that
write.csv2(SumSpecF, file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Specific_Factors_returns.csv")
# Export as TexFile
print.xtable(xtable(SumSpecF), file = "~/GitHub/R_Project/SPL-OilUS/Summary-Statistics/1 _ Summary_Specific_Factors_returns.txt")

# remove the remaining variables
rm(dataFinal, Sub1, SumCommonF, SumSpecF)

