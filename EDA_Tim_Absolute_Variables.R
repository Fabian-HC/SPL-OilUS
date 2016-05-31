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

# For this tool, we need to retrieve a written function
# see: http://www.peterhaschke.com/r/2013/04/24/MultiPlot.html
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

# Get first feel fo the panel data set
summary(data)
# Still want to find out whether I can export that in an easy fashion
# describe(data) Does not work
# For enterprise-specific information, the first 2 commands were not helpful

# Define company-specific variable set
colnames(data[, 2:7])
# That is the enterprise-specific information I would like to sumamrize
specific <- data[, 2:7]
describeBy(specific, group = specific$Company) # result can be presented better

EntSummaryMat <- describeBy(specific[, 2], group = specific$Company, mat = TRUE)
SumTable <- data.frame(EntSummaryMat)
EntSummaryMat[1, ]

EntSummaryMat <- data.frame(EntSummaryMat)
write.csv2(EntSummaryMat, file = "EntSummaryMat.csv")


# Set up a 'customized' Summary Table for the common factors
CommonF <- data.frame(subset(data[, 8:10], data$Company == 1))
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
SumMat <- round(SumMat, digits = 2)
SumMat <- data.frame(SumMat)
write.csv2(SumMat, file = "Z _ Summary_Common_Factors_Absolute.csv")


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
SumMat <- round(SumMat, digits = 2)
SumMat <- data.frame(SumMat)
write.csv2(SumMat, file = "Z _ Summary_Specific_Factors_Aggregate_Absolute.csv")








# Try to obtain the summary statistics for the specific firms
# Set up an average Table to start with


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





# Set up a histogram for all common factors
# Numbers for bins, so far, arbitarily chosen
# Pre-Step: prepare generated normal distributions
#  Histogram 1
g1 = seq(min(CommonF$Market), max(CommonF$Market), length = 100)
dens1 = dnorm(g1, mean(CommonF$Market), sd(CommonF$Market))
# Histogram 2
g2 = seq(min(CommonF$Oil), max(CommonF$Oil), length = 100)
dens2 = dnorm(g2, mean(CommonF$Oil), sd(CommonF$Oil))
# Histogram 3
g3 = seq(min(CommonF$Gas), max(CommonF$Gas), length = 100)
dens3 = dnorm(g3, mean(CommonF$Gas), sd(CommonF$Gas))

# Plot histograms and save them into a jpg-File
jpeg(filename = "Common_Factors_Histograms.jpg", pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE, height = 300, width = 1000)
par(mfrow = c(1,3)) # Set that I want the graphs to appear
# in 1 Row and 3 Columns
hist(CommonF$Market, breaks = 100, freq = FALSE)
lines(g1, dens1, col = "red")
hist(CommonF$Oil, breaks = 50, freq = FALSE)
lines(g2, dens2, col = "red")
hist(CommonF$Gas, breaks = 70, freq = FALSE)
lines(g3, dens3, col = "red")
dev.off()

# Set up Box Plots for Common Factors
jpeg(filename = "Common_Factors_Boxplots.jpg", pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE, height = 300, width = 1000)
par(mfrow = c(1,3)) # display graphs in 1 row and 3 columns
boxplot(CommonF$Market)
boxplot(CommonF$Oil)
boxplot(CommonF$Gas)
dev.off()













# Generate Histograms for Specific Factors aggregate
# SpecFAggregate - Corresponding Data Frame
variable.names(SpecFAggregate)
# Making a Histogram for Stock does not make sense

hist(SpecFAggregate$Assets.to.Market.Cap, breaks = 100, freq = FALSE)
hist(SpecFAggregate$BV.Equity.to.Market.Cap, breaks = 100, freq = FALSE)
# seems to be lognormally distributed
hist(SpecFAggregate$Debt.to.Equity, breaks = 100, freq = FALSE)
# I am sceptical regarding this data
# Looking at this graph, I wonder whether Debt/Equity is 
# measured in %-terms or not. If not, something is wrong 
# with da data

# Did some cosmetig adjustments here
hist(SpecFAggregate$Net.Income, breaks = 100, freq = FALSE, xlim = c(-2000, 5000))



# Plot histograms including a normal distribution
# Save a jpeg of this
jpeg(filename = "Summary_Specific_Factors_Aggregate_Boxplots.jpg", pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE, height = 300, width = 1000)

par(mfrow = c(2,2))
par(mar=c(3,3,3,3))

hist(SpecFAggregate$Assets.to.Market.Cap, breaks = 100, freq = FALSE)
g1 = seq(min(SpecFAggregate$Assets.to.Market.Cap), max(SpecFAggregate$Assets.to.Market.Cap), length = 100)
dens1 = dnorm(g1, mean(SpecFAggregate$Assets.to.Market.Cap), sd(SpecFAggregate$Assets.to.Market.Cap))
lines(g1, dens1)
# Cannot be normally distributed by nature of the variable
# In fact, on absolute variables, OLS or POLS cannot be used, censores / truncated variable

hist(SpecFAggregate$BV.Equity.to.Market.Cap, breaks = 100, freq = FALSE)
g1 = seq(min(SpecFAggregate$BV.Equity.to.Market.Cap), max(SpecFAggregate$BV.Equity.to.Market.Cap), length = 100)
dens1 = dnorm(g1, mean(SpecFAggregate$BV.Equity.to.Market.Cap), sd(SpecFAggregate$BV.Equity.to.Market.Cap))
lines(g1, dens1)
# Cannot be normally distributed by nature of the variable
# In fact, on absolute variables, OLS or POLS cannot be used, censores / truncated variable

hist(SpecFAggregate$Debt.to.Equity, breaks = 100, freq = FALSE)
g1 = seq(min(SpecFAggregate$Debt.to.Equity), max(SpecFAggregate$Debt.to.Equity), length = 100)
dens1 = dnorm(g1, mean(SpecFAggregate$Debt.to.Equity), sd(SpecFAggregate$Debt.to.Equity))
lines(g1, dens1)
# Cannot be normally distributed by nature of the variable
# In fact, on absolute variables, OLS or POLS cannot be used, censores / truncated variable
# NOTE THAT TAKING THE VARIABE TO: e^(VAR) CAN HELP AS THE VARIABLES
# MIGHT BE LOGNORMALLY DISTRIBUTED

hist(SpecFAggregate$Net.Income, breaks = 100, freq = FALSE, xlim = c(-2000, 5000))
g1 = seq(min(SpecFAggregate$Net.Income), max(SpecFAggregate$Net.Income), length = 100)
dens1 = dnorm(g1, mean(SpecFAggregate$Net.Income), sd(SpecFAggregate$Net.Income))
lines(g1, dens1)

dev.off()



variable.names(data)

MainText <- colnames(data)

# Generate Histograms for Specific Factors by enterprise
i = 1
while(i < 10){
  help1 <- subset(data, data$Company == i)
    NameFile <- paste("Histograms_Company", i, ".jpg", sep = "")
    jpeg(filename = NameFile, pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE, height = 400, width = 800)
    par(mfrow = c(1,5))
        j = 3
        while(j < 8){
          hist(help1[,j], breaks = 50, freq = FALSE, main = paste(MainText[j]))
          g1 = seq(min(help1[,j]), max(help1[,j]), length = 100)
          dens1 = dnorm(g1, mean(help1[,j]), sd(help1[,j]))
          j = j + 1
          print(j)
        }
    dev.off()
  
    NameFile <- paste("Boxplots_Company", i, ".jpg", sep = "")
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




# Plot all Stock Prices in 1 Plot
# Thsi is necessary as the company dummy is a factor variable
a <- max(as.numeric(unique(data$Company))) + 1
b <- nrow(Sub1)
StockSet <- matrix(NA, ncol = a, nrow = b)
StockSet <- data.frame(StockSet)
StockSet[,1] <- DateVec
VarNames <- c("Date")

i = 1
while(i < 10){
  Sub1 <- subset(data, data$Company == i)
  StockSet[,(i+1)] <- (Sub1$Stock/Sub1$Stock[1]) * 100
  a <- paste("Stock", i, sep = "")
  VarNames <- c(VarNames, a) 
  i = i + 1
}

colnames(StockSet) <- VarNames
colnames(StockSet)
head(StockSet)

a <- ncol(StockSet)
StockSet[,2:a] <- round(StockSet[,2:a], digits = 1)
head(StockSet)

# Preparing Plot
VarSetMelt <- melt(StockSet, id = "Date")
# VarSetMelt <- melt(VarSet, id = "DateVec")

# Save the data set with indexed stock data 
write.csv2(StockSet,"Stocks_Indexed.csv") # Does not work properly yet

# Plot the Indexed Stock Prices Together
dev.off() # necessary befor plotting again
jpeg(filename = "All_Stocks_Plot.jpg")
# Plot the indexed stock prices
# ggplot(data=VarSetMelt, aes(x = VarSetMelt$DateVec, y=value, colour=variable)) + geom_line()
p <- ggplot(data=VarSetMelt, aes(x = VarSetMelt$Date, y=value, colour=variable)) + geom_line()
p
dev.off()




# This Graph looks a bit messy, let's split it into 2
# Let's include only 4/5 lines per Graph, else I do not see clearly
# colnames(VarSet)
VarSetMelt1 <- melt(StockSet[,c(1,2:5)], id = "Date")
VarSetMelt2 <- melt(StockSet[,c(1,6:10)], id = "Date")


# Plot the indexed stock prices
# For first 4 stocks
jpeg(filename = "Stock1-4_Plot.jpg")
p <- ggplot(data=VarSetMelt1, aes(x = VarSetMelt1$Date, y=value, colour=variable)) 
p <- p + geom_line()
p
dev.off()

# Would be interesting to have a look at stock 3 and 2!

# plot last 5 stocks (no 5 to 9)
jpeg(filename = "Stock5-9_Plot.jpg")
p <- ggplot(data=VarSetMelt2, aes(x = VarSetMelt2$Date, y=value, colour=variable)) 
p <- p + geom_line()
p
dev.off()

# Stock 8 looks interesting in the sense that its pattern differs most
# wrt to the other stocks
# Stock 6 exhibits a lot of fluctuations
# and we should check out information on this one
# Stock 7 could be interesting as it performed pretty well after 2008






# Plot all Debt to Equity in 1 Plot
# Thsi is necessary as the company dummy is a factor variable
a <- max(as.numeric(unique(data$Company))) + 1
b <- nrow(Sub1)
D_E_Set <- matrix(NA, ncol = a, nrow = b)
D_E_Set <- data.frame(D_E_Set)
D_E_Set[,1] <- DateVec
VarNames <- c("Date")

i = 1
while(i < 10){
  Sub1 <- subset(data, data$Company == i)
  D_E_Set[,(i+1)] <- Sub1$Debt.to.Equity
  a <- paste("Firm", i, sep = "_Debt_to_Equity")
  VarNames <- c(VarNames, a) 
  i = i + 1
}

colnames(D_E_Set) <- VarNames
colnames(D_E_Set)
head(D_E_Set)
# Must be percentage values, else that does not make sense

a <- ncol(D_E_Set)
D_E_Set[,2:a] <- round(D_E_Set[,2:a], digits = 2)
head(D_E_Set)

# Preparing Plot
VarSetMelt <- melt(D_E_Set, id = "Date")
# VarSetMelt <- melt(VarSet, id = "DateVec")

# Save the data set with indexed stock data 
write.csv2(D_E_Set,"Debt_to_Equity_Firms.csv") # Does not work properly yet

# Plot the Indexed Stock Prices Together
dev.off() # necessary befor plotting again
jpeg(filename = "Debt_to_Equity_Firms.jpg")
# Plot the indexed stock prices
# ggplot(data=VarSetMelt, aes(x = VarSetMelt$DateVec, y=value, colour=variable)) + geom_line()
p <- ggplot(data=VarSetMelt, aes(x = VarSetMelt$Date, y=value, colour=variable)) + geom_line()
p
dev.off()

# We should exclude firm 3 as it was levered up to the kink in 2005
# This will be done in the first following plot

# I'd also exclude firm 9 because it is also highly levered int he 90s
# This will be done in the second following plot
VarSetMelt1 <- melt(D_E_Set[,-4], id = "Date")
VarSetMelt2 <- melt(D_E_Set[,-c(4,10)], id = "Date")
VarSetMelt3 <- melt(D_E_Set[,-c(4,9,10)], id = "Date")
VarSetMelt4 <- melt(D_E_Set[,-c(3,4,8,9,10)], id = "Date")

# Looking at the second following plot, I noticed that it 
#cmight also make
# sense to exclude firm 8 as it is also highly levered in the
# 200s


# Plot Excluding Firm 3
jpeg(filename = "Debt_to_Equity_Firms_EX3.jpg")
p <- ggplot(data=VarSetMelt1, aes(x = VarSetMelt1$Date, y=value, colour=variable)) 
p <- p + geom_line()
p
dev.off()


# Plot Excluding Firm 3 and 9
jpeg(filename = "Debt_to_Equity_Firms_EX3_9.jpg")
p <- ggplot(data=VarSetMelt2, aes(x = VarSetMelt2$Date, y=value, colour=variable)) 
p <- p + geom_line()
p
dev.off()

# Plot Excluding Firm 3,8 9
jpeg(filename = "Debt_to_Equity_Firms_EX3_8_9.jpg")
p <- ggplot(data=VarSetMelt3, aes(x = VarSetMelt3$Date, y=value, colour=variable)) 
p <- p + geom_line()
p
dev.off()

# Plot Excluding Firm 2,3,7,8 9
jpeg(filename = "Debt_to_Equity_Firms_EX2_3_7_8_9.jpg")
p <- ggplot(data=VarSetMelt4, aes(x = VarSetMelt4$Date, y=value, colour=variable)) 
p <- p + geom_line()
p
dev.off()


# Plot D/E of firm 2
jpeg(filename = "Debt_to_Equity_Firm_2.jpg")
p <- ggplot(data=D_E_Set, aes(x = D_E_Set$Date, y= Firm_Debt_to_Equity2)) 
p <- p + geom_line()
p
dev.off()


# Plot D/E of firm 3
jpeg(filename = "Debt_to_Equity_Firm_3.jpg")
p <- ggplot(data=D_E_Set, aes(x = D_E_Set$Date, y= Firm_Debt_to_Equity3)) 
p <- p + geom_line()
p
dev.off()

# Plot D/E of firm 7
jpeg(filename = "Debt_to_Equity_Firm_7.jpg")
p <- ggplot(data=D_E_Set, aes(x = D_E_Set$Date, y= Firm_Debt_to_Equity7)) 
p <- p + geom_line()
p
dev.off()


# Plot D/E of firm 8
jpeg(filename = "Debt_to_Equity_Firm_8.jpg")
p <- ggplot(data=D_E_Set, aes(x = D_E_Set$Date, y= Firm_Debt_to_Equity8)) 
p <- p + geom_line()
p
dev.off()

# Plot of firm 9
jpeg(filename = "Debt_to_Equity_Firm_9.jpg")
p <- ggplot(data=D_E_Set, aes(x = D_E_Set$Date, y= Firm_Debt_to_Equity9)) 
p <- p + geom_line()
p
dev.off()



# I consider that it would be interesting to look at what 
# recently occured to firm 2


# Plot excluding firm 2 and 7


# Plot the oil and the gas price in seperate plots
Oil <- subset(data$Oil, data$Company == 1)
Gas <- subset(data$Gas, data$Company == 1)
Market <- subset(data$Market, data$Company == 1)

PlotOilGas <- data.frame(DateVec, Oil, Gas, Market)
p1 <- ggplot(PlotOilGas) + geom_line(aes(x = DateVec, y = Oil))
p2 <- ggplot(PlotOilGas) + geom_line(aes(x = DateVec, y = Gas))
p3 <- ggplot(PlotOilGas) + geom_line(aes(x = DateVec, y = Market))

multiplot(p1, p2, p3, cols=2)

jpeg(filename = "Oil_Gas_Market.jpg", pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE, height = 400, width = 800)
multiplot(p1, p2, p3, cols=2)
dev.off()


# 




# Compare companies financial condition nowadays
# Can be done more elegantly resorting to the "subset()" function
colnames(data)
# Compare Current Assets/Market Cap
Company <- c(1:9)

a <- data$Assets.to.Market.Cap[79]
Assets_Mcap <- data$Assets.to.Market.Cap[79]

i = 2
while(i < 10){
  a <- data$Assets.to.Market.Cap[i * 79]
  Assets_Mcap <- c(Assets_Mcap, a)  
  i = i + 1
  print (i)
}
# Company
# class(Company)
# Assets_Mcap
ma <- cbind(Company, Assets_Mcap)
ma <- data.frame(ma)
ma$Company <- as.factor(ma$Company)
ggplot(ma, aes(x=Company, y=Assets_Mcap)) + geom_bar(stat="identity")



# Plot the oil and the gas price in seperate plots
Oil <- subset(data$Oil, data$Company == 1)
Gas <- subset(data$Gas, data$Company == 1)
Market <- subset(data$Market, data$Company == 1)

PlotOilGas <- data.frame(DateVec, Oil, Gas, Market)
p1 <- ggplot(PlotOilGas) + geom_line(aes(x = DateVec, y = Oil))
p2 <- ggplot(PlotOilGas) + geom_line(aes(x = DateVec, y = Gas))
p3 <- ggplot(PlotOilGas) + geom_line(aes(x = DateVec, y = Market))

multiplot(p1, p2, p3, cols=2)

jpeg(filename = "Oil_Gas_Market.jpg", pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE, height = 400, width = 800)
multiplot(p1, p2, p3, cols=2)
dev.off()

# ggsave(filename = "Oil_and_Gas_Price.pdf", height = 6, width = 6) # the pdf export works, so that is save



# Let's work with the return data








# NOT USED CODE SECTION




Stock1 <- data$Stock[1:t]
Stock2 <- data$Stock[(t+1):(2*t)]
Stock3 <- data$Stock[(2*t+1):(3*t)]
Stock4 <- data$Stock[(3*t+1):(4*t)]
Stock5 <- data$Stock[(4*t+1):(5*t)]
Stock6 <- data$Stock[(5*t+1):(6*t)]
Stock7 <- data$Stock[(6*t+1):(7*t)]
Stock8 <- data$Stock[(7*t+1):(8*t)]
Stock9 <- data$Stock[(8*t+1):(9*t)]

# Stock1[1]
# DateVec[1]

# Aktien Indexieren
Stock1Ind <- Stock1/Stock1[1]
Stock2Ind <- Stock2/Stock2[1]
Stock3Ind <- Stock3/Stock3[1]
Stock4Ind <- Stock4/Stock4[1]
Stock5Ind <- Stock5/Stock5[1]
Stock6Ind <- Stock6/Stock6[1]
Stock7Ind <- Stock7/Stock7[1]
Stock8Ind <- Stock8/Stock8[1]
Stock9Ind <- Stock9/Stock9[1]

# DateVec[1]
# Stock1Ind[1]

VarSet <- data.frame(DateVec, Stock1Ind, Stock2Ind, Stock3Ind, Stock4Ind, Stock5Ind, Stock6Ind, Stock7Ind, Stock8Ind, Stock9Ind)
VarSetMelt <- melt(VarSet, id = "DateVec")
# Save the data set with indexed stock data 
write.csv2(VarSet,"Stocks_Indexed.csv")


# Plot the Indexed Stock Prices Together
dev.off() # necessary befor plotting again
jpeg(filename = "All_Stocks_Plot.jpg")
# Plot the indexed stock prices
# ggplot(data=VarSetMelt, aes(x = VarSetMelt$DateVec, y=value, colour=variable)) + geom_line()
p <- ggplot(data=VarSetMelt, aes(x = VarSetMelt$DateVec, y=value, colour=variable)) + geom_line()
p
dev.off()
