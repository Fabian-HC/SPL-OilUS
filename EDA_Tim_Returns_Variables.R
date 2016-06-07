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
# Handwritten function for later on

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# For my customized summary later on
SumCustom <- function(x){
  r1 = mean(x)
  r2 = median(x)
  r3 = sd(x)
  r4 = min(x)
  r5 = max(x)
  
  return(matrix(c(r1, r2, r3, r4, r5), ncol = 5))
         
}


# For this tool, we need to retrieve a written function
# see: http://www.peterhaschke.com/r/2013/04/24/MultiPlot.html
library(lattice)
library("reshape2")






setwd("C:/Users/Trimme/Documents/Studium Master/Kurse/2016 SoSe/Statistical Programming Languages/Dataset")
data <- read.csv2("Dataset-FINALupdated_returns.csv", stringsAsFactors=FALSE)


# Convert Date such that R recognizes it as date
class(data$Date)
data$Date <- as.Date(data$Date, format = "%d.%m.%Y")
class(data$Date)

#change company number -> name
data$Company = as.character(data$Company)
data$Company[data$Company== "1"] ="Exxon Mobil Corp"
data$Company[data$Company== "2"] ="Apache Corp"
data$Company[data$Company== "3"] ="CenterPoint Energy Inc"
data$Company[data$Company== "4"] ="Chevron Corp"
data$Company[data$Company== "5"] ="Horizons Enhanced"
data$Company[data$Company== "6"] ="Murphy Oil Corporation"
data$Company[data$Company== "7"] ="Occidental Petroleum Corp"
data$Company[data$Company== "8"] ="PG&E Corp"
data$Company[data$Company== "9"] ="Williams Cos Inc"

# Generate and store date vector for later purposes
DateVec <- subset(data$Date, data$Company == "Exxon Mobil Corp")
#   data$Date[1:79]
class(DateVec)
DateVec <- sort(DateVec)
# DateVec

head(data)
tail(data)
# We see that there are missing values at the end of the dataset
# This is because we lost 1 observation per Company as we set up
# the returns

row.has.na <- apply(data, 1, function(x){any(is.na(x))})
sum(row.has.na)
if(sum(row.has.na) != 0){
  data <- data[!row.has.na,]
}
tail(data)

# Convert data into panel data
data <- pdata.frame(data, index = c("Company", "Date"), drop.index = FALSE, row.names = TRUE) 

# Has R read in the panel Data set with Date as date variable?
class(data$Date)
# If not, let's change that
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
class(data$Date)
# Done

# Get first feel fo the panel data set
summary(data)

# Set up a 'customized' Summary Table for the common factors
CommonF <- data.frame(subset(data[, 8:10], data$Company == "Exxon Mobil Corp"))
# a <- nrow(CommonF) # Not used here
# variable.names(CommonF) # not used here
# Note that variable Names are correctly copied!
SumMat <- matrix(NA, ncol = 5, nrow = 3)
StatInterest <- c("mean", "median", "sd", "min", "max")
colnames(SumMat) <- StatInterest
rownames(SumMat) <- variable.names(CommonF)
i = 1
while(i < 4){
  SumMat[i,] <- SumCustom(CommonF[,i])
  i = i + 1
}
# SumMat - Uncomment to take a look at it in R
SumMat <- round(SumMat, digits = 4)
SumMat <- data.frame(SumMat)
write.csv2(SumMat, file = "Z _ Summary_Common_Factors_Returns.csv")



# Set up a summary table for the specific factors in aggregate

SpecFAggregate <- data[, 3:7]
SumMat <- matrix(NA, ncol = 5, nrow = 5)
StatInterest <- c("mean", "median", "sd", "min", "max")
colnames(SumMat) <- StatInterest
rownames(SumMat) <- variable.names(SpecFAggregate)

i = 1
while(i < 6){
  SumMat[i,] <- SumCustom(SpecFAggregate[,i])
  i = i + 1
}
# SumMat - Uncomment to take a look at it in R
SumMat <- round(SumMat, digits = 4)
SumMat <- data.frame(SumMat)
write.csv2(SumMat, file = "Z _ Summary_Specific_Factors_Aggregate_Returns.csv")



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
  Filename <- paste("Specific_Factors_Company_", i, "_Returns.csv", sep = "" )
  write.csv2(SumMat1, file = Filename)
  rm(Sub1)
  rm(SumMat1)
  i = i + 1
}



# Generate Histograms for Specific Factors by enterprise
MainText <- colnames(data)
i = 1
while(i < 10){
  help1 <- subset(data, data$Company == i)
  NameFile <- paste("Histograms_Company_Returns", i, ".jpg", sep = "")
  jpeg(filename = NameFile, pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE, height = 400, width = 800)
  par(mfrow = c(1,5))
  j = 3
  while(j < 8){
    hist(help1[,j], breaks = 50, freq = FALSE, main = paste(MainText[j]))
    g1 = seq(min(help1[,j]), max(help1[,j]), length = 100)
    dens1 = dnorm(g1, mean(help1[,j]), sd(help1[,j]))
    lines(g1, dens1)
    
    j = j + 1
    print(j)
  }
  dev.off()
  
  NameFile <- paste("Boxplots_Company_Returns", i, ".jpg", sep = "")
  jpeg(filename = NameFile, pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE, height = 400, width = 800)
  par(mfrow = c(1,5))
  j = 3
  while(j < 8){
    boxplot(help1[,j], main = paste(MainText[j]))
    j = j + 1
    print(j)
  }
  dev.off()
  i = i + 1
  print(i)
}










# UNUSED CODE SECTIONS / FAILS

# Obtain summary of Common Factors via Loop - did not work
#StatInterest <- c("mean", "median", "sd", "min", "max")
#SumMat <- matrix(NA, ncol = 5, nrow = 3)
# Loop intent not working so far... :(

# head(CommonF)
# colnames(CommonF)
# head(CommonF)
# head(CommonF[,1])
# class(CommonF[,2])
# StatInterest <- apply(CommonF[,2:3], MARGIN = 2, FUN = SumCustom(CommonF))


#i = 1
#while(i < 6){
# StatInterest <- apply(CommonF, MARGIN = 2, FUN = SumCustom(CommonF))
#
#  SumMat[,i] <- matrix(StatInterest, nrow = 3)
#  print(StatInterest[i])
#  i = i + 1
#}
