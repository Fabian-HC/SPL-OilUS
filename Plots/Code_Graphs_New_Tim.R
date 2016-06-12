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

plotlines = function(x, y){
  lines(x, y, col =  "red")
}

# For this tool, we need to retrieve a written function
# see: http://www.peterhaschke.com/r/2013/04/24/MultiPlot.html
library(lattice)
library("reshape2")
library(ggplot2)

histfun = function(x){
  # bw = diff(range(x)) / (2 * IQR(x) / length(x)^(1/3))
  g1 = seq(min(x), max(x), length = 100)
  dens1 = dnorm(g1, mean(x), sd(x))
  hist(x, freq = FALSE)
  lines(g1, dens1, col = "red")
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

# Read in the data frame
setwd("C:/Users/Trimme/Documents/GitHub/R_Project/SPL-OilUS")
# setwd("C:/Users/Trimme/Documents/Studium Master/Kurse/2016 SoSe/Statistical Programming Languages/Dataset", st)
data = read.csv2("~/GitHub/R_Project/SPL-OilUS/Data-Set/Dataset-FINALupdated_absolute.csv", stringsAsFactors = FALSE)


# Convert Date such that R recognizes it as date
# Note that, for some reason, we have to do this twice
# Later on as well, when we sort the panel data
data$Date <- as.Date(data$Date, format = "%d.%m.%Y")
class(data$Date)

# Generate and store date vector for later purposes
DateVec <- data.frame(subset(data$Date, data$Company == 1))
# Sort this vector
DateVec = sort(DateVec)

# Convert data into panel data
data <- pdata.frame(data, index = c("Company", "Date"), drop.index = F, row.names = T) 
# Has R read in the panel Data set with Date as date variable?
class(data$Date)
# If not, let's change that
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
class(data$Date)
# Done

# Plot all Stock Price evolvements in 1 Plot
# Thsi is necessary as the company dummy is a factor variable
a <- max(as.numeric(unique(data$Company))) + 1
b <- nrow(data[data$Company == 1,])
StockSet <- matrix(NA, ncol = a, nrow = b)
StockSet <- data.frame(StockSet)


i = 1
while(i < 10){
  if(i == 1){
    Sub1 = subset(data, data$Company == 1)
    StockSet[,1:2] = Sub1[,c(1,3)]
    StockSet[,2] = StockSet[,2]/StockSet[1,2]*100
    a <- paste("Stock", i, sep = "")
    VarNames = c("Date", a) 
  }else{
  Sub1 <- subset(data, data$Company == i)
  SubStock = Sub1[,3]
  SubStock = SubStock/SubStock[1] * 100
  StockSet[,(i+1)] = SubStock
  a <- paste("Stock", i, sep = "")
  VarNames <- c(VarNames, a) 
  }
  i = i + 1
}

colnames(StockSet) <- VarNames
colnames(StockSet)
# head(StockSet)
# Save the data set with indexed stock data and round the numbers
# a <- ncol(StockSet)
# StockSet[,2:a] <- round(StockSet[,2:a], digits = 1)
# head(StockSet)
# write.csv2(StockSet,"Stocks_Indexed.csv") # Does not work properly yet

# We can indeed plot all stocks in 1 graph without ggplot. Yet the legend will be missing
plot(StockSet$Date, StockSet$Stock1, type = "l", ylim = c(0,1200), ylab = "Stock Index (1996 = 100)")
LegendNames = "Stock_1"
par(xpd=TRUE)
i = 2
ColVec = c("black", "red", "yellow", "green", "violetred", "blue", "antiquewhite4", "deeppink", "gold4")
while(i < 9){
  lines(StockSet$Date, StockSet[,(i+1)], col = ColVec[i-1])
  LegendNames = cbind(LegendNames, paste("Stock_", i, sep = ""))
  i = i + 1
}


# Preparing Plot - Using GGPLOT
VarSetMelt <- melt(StockSet, id = "Date")
# Plot the Indexed Stock Prices Together
# dev.off() # necessary befor plotting again
#jpeg(filename = "All_Stocks_Plot.jpg")
# Plot the indexed stock prices
# ggplot(data=VarSetMelt, aes(x = VarSetMelt$DateVec, y=value, colour=variable)) + geom_line()
p <- ggplot(data=VarSetMelt, aes(x = VarSetMelt$Date, y=value, colour=variable)) + geom_line()
p
# dev.off()


Sub1 = subset(data, data$Company == 1)
# Plot Oil, Gas and Market beside one and another
#jpeg(filename = "Common_Factors_Development.jpg")
par(mfrow = c(1,3))
plot(Sub1$Date, Sub1$Oil, type = "l", xlab = "Year", ylab = "Oil Price")
plot(Sub1$Date, Sub1$Gas, type = "l", xlab = "Year", ylab = "Gas Price")
plot(Sub1$Date, Sub1$Market, type = "l", xlab = "Year", ylab = "Dow Jones Index")
# dev.off()

# Plot each company with specific and common factors
CompanyNames <- levels(data$Company) # Extract the 'Company Names'
VarNames <- colnames(data[,3:10])

par(mfrow = c(1,8)) # Set that I want to plot 8 graphs beside one and another
par(mar=c(2,2,2,2)) # Insert margins between the different plots. 
# Note that I would have to alter this conficurations when setting up other plots later
#dev.off()
i = 1
while(i < 10){
  NameFile <- paste("Company_", i,"_SpecificFact.jpg", sep = "")
  jpeg(filename = NameFile, pointsize =24, quality = 200, bg = "white", res = NA, restoreConsole = TRUE, height = 400, width = 3600)
  par(mfrow = c(1,8))
  Sub1 <- subset(data, data$Company == i)
  j = 3
  while(j < 11){
    Filename <- paste("Company_", CompanyNames[i],"_", VarNames[j], ".jpeg", sep = "")
    Yvalues <- Sub1[,j]
    plot(Sub1$Date, Yvalues, type = "l", ylab = paste(VarNames[(j - 2)]) , main = paste("C_", i, sep = ""))
    j = j + 1  
  }
  dev.off()
  i = i + 1
  # dev.off() - apparently not needed
}
# dev.off() - apparently not needed


# since we switch to another dataset, remove 
# all the stuff we stored so far
rm(list = ls())

# Plotting the histograms of the specific factors 
# after we have transformed them into returns
# Load the respective dataset
data = read.csv2("~/GitHub/R_Project/SPL-OilUS/Data-Set/Dataset-FINALupdated_returns.csv", stringsAsFactors = FALSE)

# Convert Date such that R recognizes it as date
data$Date <- as.Date(data$Date, format = "%d.%m.%Y")
class(data$Date)

# Generate and store date vector for later purposes
DateVec <- subset(data$Date, data$Company == 1)
DateVec <- sort(DateVec)
# DateVec
head(data)
tail(data)
# We see that there are missing values at the end of the dataset
# This is because we lost 1 observation per Company as we set up
# the returns. Remove the missing values

row.has.na <- apply(data, 1, function(x){any(is.na(x))})
sum(row.has.na)
if(sum(row.has.na) != 0){
  data <- data[!row.has.na,]
}
tail(data)
# we managed to remove the missing observations


# Convert data into panel data
data <- pdata.frame(data, index = c("Company", "Date"), drop.index = F, row.names = T) 
# Has R read in the panel Data set with Date as date variable?
class(data$Date)
# If not, let's change that
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
class(data$Date)
# Done

MainText = variable.names(data)


# Generate Histograms for Specific Factors by enterprise
# Now with one Loop less!!!
i = 1
while(i < 10){
  help1 <- subset(data, data$Company == i)
  NameFile <- paste("Apply_Histograms_Company", i, "_returns.jpg", sep = "")
  jpeg(filename = NameFile, pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE, height = 400, width = 800)
  par(mfrow = c(1,5))
  apply(help1[,3:7], MARGIN = 2, FUN = histfun)
  dev.off()
  i = i + 1
  #print(i)
}





# Old version with one loop more!!!
# Generate Histograms for Specific Factors by enterprise
i = 1
while(i < 10){
  help1 <- subset(data, data$Company == i)
  #NameFile <- paste("Histograms_Company", i, "_returns.jpg", sep = "")
  #jpeg(filename = NameFile, pointsize =12, quality = 200, bg = "white", res = NA, restoreConsole = TRUE, height = 400, width = 800)
  par(mfrow = c(1,5))
  j = 3
  while(j < 8){
    bw = diff(range(help1[,j])) / (2 * IQR(help1[,j]) / length(help1[,j])^(1/3))
    hist(help1[,j], breaks = bw, freq = FALSE, main = paste(MainText[j]))
    g1 = seq(min(help1[,j]), max(help1[,j]), length = 100)
    dens1 = dnorm(g1, mean(help1[,j]), sd(help1[,j]))
    lines(g1, dens1, col = "red")
    # I still have to program that R sets the y-limits adequately
    # In particular, the maximum
    j = j + 1
    # print(j)
  }
  # dev.off()
i = i + 1
#print(i)
}



