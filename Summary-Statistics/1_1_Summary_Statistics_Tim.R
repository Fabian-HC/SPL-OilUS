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
getwd()
# setwd("C:/Users/Trimme/Documents/Studium Master/Kurse/2016 SoSe/Statistical Programming Languages/Dataset", st)
load("./Data-Set/InitialData_Panel_Date_OK_OLD_ZScore.RData", verbose = FALSE)

# Summary statistics for common factors
Sub1 = subset(data, data$Company == levels(data$Company)[1])
SumCommonF = describe(Sub1[,8:ncol(data)], skew = TRUE, trim = 0, type = 1)
SumCommonF = round(SumCommonF[,-c(1,2,6,7,10,13)], digits = 2)
# Export as CSV
write.csv2(SumCommonF, file = "./Summary-Statistics/1 _ Summary_Common_Factors_Absolute.csv")
# Export as TexFile
print.xtable(xtable(SumCommonF), file = "./Summary-Statistics/1 _ Summary_Common_Factors_Absolute.txt")

rm(SumCommonF, Sub1)

# Summary statistics of company-specific variables
SumSpecF = describeBy(data[,2:7], group = "Company", mat = TRUE, digits = 2, trim = 0, type = 1)
SumSpecF = SumSpecF[-c(1:9),]
SumSpecF = SumSpecF[,-c(4,8,9,12,15)]
SumSpecF = SumSpecF[order(SumSpecF$group1, SumSpecF$vars),]
SumSpecF$vars = factor(SumSpecF$vars)
levels(SumSpecF$vars) = colnames(data[,3:7])
SumSpecF = SumSpecF[,-1]
rownames(SumSpecF) = NULL
write.csv2(SumSpecF, file = "./Summary-Statistics/1 _ Summary_Specific_Factors_Absolute.csv")
# Export as TexFile
print.xtable(xtable(SumSpecF), file = "./Summary-Statistics/1 _ Summary_Specific_Factors_Absolute.txt")


rm(data, SumSpecF)
#-------------------------------------------------------------
#-------------------------------------------------------------
# Summary statistics for return verion of variables
load("./Data-Set/For_Marcus_OK_Old_Version.RData", verbose = TRUE)
# Summary statistics for common factors
Sub1 = subset(data, data$Company == levels(data$Company)[1])
SumCommonF = describe(Sub1[,8:ncol(data)], skew = TRUE, trim = 0, type = 1)
SumCommonF = round(SumCommonF[,-c(1,2,6,7,10,13)], digits = 2)
# Export as CSV
write.csv2(SumCommonF, file = "./Summary-Statistics/1 _ Summary_Common_Factors_returns.csv")
# Export as TexFile
print.xtable(xtable(SumCommonF), file = "./Summary-Statistics/1 _ Summary_Common_Factors_returns.txt")

rm(Sub1, SumCommonF)

# Summary statistics of company-specific variables
SumSpecF = describeBy(data[,2:7], group = "Company", mat = TRUE, digits = 2, trim = 0, type = 1)
SumSpecF = SumSpecF[-c(1:9),]
SumSpecF = SumSpecF[,-c(4,8,9,12,15)]
SumSpecF = SumSpecF[order(SumSpecF$group1, SumSpecF$vars),]
SumSpecF$vars = factor(SumSpecF$vars)
class(SumSpecF$vars)
levels(SumSpecF$vars) = colnames(data[,3:7])
SumSpecF = SumSpecF[,-1]
rownames(SumSpecF) = NULL
write.csv2(SumSpecF, file = "./Summary-Statistics/1 _ Summary_Specific_Factors_returns.csv")
# Export as TexFile
print.xtable(xtable(SumSpecF), file = "./Summary-Statistics/1 _ Summary_Specific_Factors_returns.txt")

# remove the remaining variables
rm(data, SumSpecF)
