# Let's see if I can make changes and upload them!!!


# remove variables
rm(list = ls())

# reset graphics
graphics.off()



libraries = c("stats", "graphics", "timeSeries", "reshape2", "ggplot2", "ggplot")

# libraries = c("plyr","dplyr","data.table", "tseries", "xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
# Package loading done - variable can be deleted
rm(libraries)


histfun = function(x){
  # bw = diff(range(x)) / (2 * IQR(x) / length(x)^(1/3))
  g1 = seq(min(x), max(x), length = 100)
  dens1 = dnorm(g1, mean(x), sd(x))
  maxHist = c(max(x), max(dens1))
  hist(x, freq = FALSE, main = NULL, yaxt = 'n', xaxt = 'n')
  lines(g1, dens1, col = "red", lwd = 2)
}

plotfun = function(x){

  Name = colnames(x) # we cannot extract the column name in aggregate and neither
  # in apply - that is why we decided to still resort to the loop
  YMinMax = c(min(x)*0.9, max(x)*1.1)
  p = plot(x, type = "l", xaxt = 'n', xlab = "", ylab = "", ylim = YMinMax)
  # yaxp = c(YMinMax, 5) helps regarding the ticks, but the numbers are not round
  # , main = paste("Company", i, sep = "")
}

indexFUN = function(x){
  NObs = length(x)
  Index = x/x[1]*100
  Index = round(Index, digits = 2)
  # perform indexation - base: 1996-06-28
  # Index = as.numeric(Index)
}

# Set output working directory for the graphs
getwd()
# setwd("C:/Users/Trimme/Documents/GitHub/R_Project/SPL-OilUS/Graphs_Final")

# Load the RData Set
# load("~/GitHub/R_Project/SPL-OilUS/Data-Set/InitialData_Panel_Date_OK_Companynames.RData")
data = read.csv2("Dataset-FINALupdated_returns.csv", stringsAsFactors = FALSE)
data = read.csv2("./Data-Set/Dataset-FINALupdated_absolute.csv", stringsAsFactors = FALSE)

data = data[,1:2]

write.csv2(data, file = "./Test_by_Trimme.csv")

# Obtain all stock performances and Index them
# Base: 1996-06-28 = 100%
StockSet = data.frame(t(as.matrix(
           aggregate(data$Stock, by = list(data$Company), 
           FUN = indexFUN, simplify = TRUE))))
# Unfortunately, R spits out the transformed variables as factors
# and not numbers [and as a matrix in the wrong format]
# So I applied
# (1) transforming the unordered output into a matrix - as.matrix
# (2) The matrix is given in a (Company(row), Time(column)) dimension
#     But I would like to get it in a (Time(row), Comany(column)) dimention
#     So, transform the matrix
# (3) for plotting multiple lines, we probably need to use a data frame
# (4) The elements in the data frame are factors, however, we know that 
#     they are numbers, so convert each element via apply and as.numeric
StockSet = StockSet[-1,]

# On some occasions, the previous transformations yielded the correct values
# , but provided them as character variables. Just in case that this still happens
if(class(StockSet[,1]) != "numeric"){
  StockSet = data.frame(apply(StockSet, MARGIN = 2, FUN = as.numeric))
}else{
  print("Indexed stock performance data is already numeric")
}

colnames(StockSet) = levels(data$Company)

# Save the stock set data
save(data, file="~/GitHub/R_Project/SPL-OilUS/Data-Set/Stock_Set_Indexed.RData")

DateVec = subset(data$Date, data$Company == levels(data$Company)[1])
# The same story with ggplot
# DateVec = sort(DateVec)
StockSet = cbind(DateVec, StockSet)
# Set the variable Names
# StockNames = c("Exxon", "Apache", "C. Point", "Chevron", "Hess", "Murphy Oil", "Occidental", "PG&E", "Williams")
colnames(StockSet) = c("Date", colnames(StockSet[,-1]))
# Preparing Plot - Using GGPLOT
VarSetMelt <- melt(StockSet, id = "Date")
# VarSetMelt[VarSetMelt$variable == "X1"] = "Exxon"
# Plot the Indexed Stock Prices Together
dev.off() # necessary before plotting again
# jpeg(filename = "All_Stocks_Plot.jpg")
# Plot the indexed stock prices
pdf(file = "All_Stocks_plot.pdf", height = 3, width = 5.25)
p <- ggplot(data=VarSetMelt, aes(x = VarSetMelt$Date, y=value, colour=variable)) + geom_line(size = 0.75)
# add legend configurations
p = p + theme(legend.position="bottom", legend.title=element_text(size = 0, colour = "white"))
# p = p + theme(legend.title=element_text(size = 0, colour = "white"))
p = p + theme(panel.background = element_rect(fill="white"), axis.line = element_line(colour = "black"))
p = p + xlab("Year") + ylab("Index 1996 = 100")
# Adjust appearence of axis titles (x-axis)
p = p + theme(axis.title.x = element_text(size = rel(0.8)))
# Adjust appearence of axis titles (y-axis)
p = p + theme(axis.title.y = element_text(size = rel(0.8))) # Adjust appearence of axis titles (x-axis))))
# Adjust appearence of axis tick labels
p = p + theme(axis.text = element_text(colour = "black", size = rel(0.7)))
p = p + theme(text = element_text(size=9))
p = p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1),"cm"))
# Adjust the colors
p = p + scale_color_manual(values=c("deeppink", "black", "darkgreen", "darkblue", "darkred", "darkgrey", "burlywood4", "darkmagenta", "darkgoldenrod1"  ))
p
dev.off()
# remove help variables used
rm(StockSet, VarSetMelt, StockNames, p, DateVec)
dev.off()


#  Plot Oil, Gas and Market beside one and another
Sub1 = subset(data, data$Company == levels(data$Company)[1])
Sub1 = Sub1[,c(1,8:11)]
#  Plot Oil, Gas and Market beside one and another
# jpeg(filename = "Common_Factors_Development.jpg", height = 400, width = 1000)
pdf(file = "Common_Factors_Development.pdf", height = 4, width = 10)
par(mfrow = c(1,4))
par(cex = 0.95)
par(mar = c(1,2,1.5,1), lwd = 2)
apply(Sub1[,2:5], MARGIN = 2, FUN = plotfun)
dev.off()

# Yet I hold that with Titles explaining what is what, 
# This looks way better
# jpeg(filename = "Common_Factors_Development_better.jpg", height = 400, width = 1000)
pdf(file = "Common_Factors_Development_better.pdf", height = 4, width = 10)
VarNames = colnames(Sub1)
par(mfrow = c(1,4))
par(cex = 0.95)
par(mar = c(2,2,1.5,1), lwd = 2)
i = 2
while(i < (ncol(Sub1) + 1)){
  YMinMax = c(min(Sub1[,i])*0.9, max(Sub1[,i])*1.1)
  plot(Sub1$Date, Sub1[,i], type = "l", ylim = YMinMax, main = VarNames[i], xlab = "", ylab = "")
  i = i + 1
}
dev.off()
# remove auxiliary variables used
rm(VarNames, i, Sub1, YMinMax)

# Plot each company with specific and common factors

# Note that I would have to alter this conficurations when setting up other plots later
#dev.off()
# jpeg(filename = "Companies_Specific and common factors.jpg", width = 3000, height = 1800)
# In PDF, this looks really baaaad! 
# try the company-wise loop
pdf(file = "Companies_Specific_and_common_factors.pdf", height = 8, width =11,5)
par(mfcol = c(9,9))
par(mar = c(1,1,1,1))
par(cex = 1.3)
aggregate(data[,3:11], by = list(data$Company), FUN = plotfun, simplify = FALSE)
dev.off()

# Version with loops, but more presentable output
# If necessary, load data set with comapany numbers, instead of names


# variable.names(data[,4:6]) = c("Assets/MV", "BV(EQ)/MV", "D / E")
# jpeg(filename = paste("Company_", i, "_Specific_and_Common_Factors.jpg"), width = 2100, height = 300)
VarNames = colnames(data)
i = 1
while(i < 10){
  # pdf(file = paste(levels(data$Company)[i], "_Specific_and_Common_Factors.pdf", sep = ""), height = 2.35, width =11)
  pdf(file = paste("Company_", i, "_Specific_and_Common_Factors.pdf", sep = ""), height = 2.35, width =11)
  # Adjust mfrow(nrow, ncol) for getting plot in different arragement
  par(mfrow = c(1,9), cex = 0.8, cex.main = 0.85, cex.axis = 0.7)
  Sub1 = subset(data, data$Company == levels(data$Company)[i])
  j = 3
  while(j < 12){
    if(j < 11){
      par(mar = c(2,2,2,0))
    }else{
      par(mar = c(2,2,2,0.7))
    }
    YMinMax = c(min(Sub1[,j])*0.9, max(Sub1[,j])*1.1)
    plot(Sub1$Date, Sub1[,j], type = "l", ylim = YMinMax, main = paste(levels(data$Company)[i], "\n", VarNames[j], sep = ""), xlab = "", ylab = "")
    j = j + 1
  }
  dev.off()  
  i = i + 1 
}
dev.off()



# since we switch to another dataset, remove 
# all the stuff we stored so far
rm(i, j, VarNames, YMinMax, data)

load("~/GitHub/R_Project/SPL-OilUS/Data-Set/TransformedDate.RData", verbose = TRUE)
class(dataFinal$Date)
data = dataFinal
rm(dataFinal)
MainText = variable.names(data)


# Generate Histograms for Specific Factors by enterprise
# Now without any loop!!!
# jpeg(filename = "Companies_Specific_factors_Histograms.jpg", width = 3000, height = 1800)
pdf(file = "Companies_Specific_factors_returns_Histograms.pdf", height = 6.1, width = 11)
par(mfcol = c(9,5))
par(mar = c(1,1,1,1))
par(cex = 1)
par(lwd = 2)
aggregate(data[,3:7], by = list(data$Company), FUN = histfun, simplify = FALSE)
dev.off()

# Generate histograms for common factors
pdf(file = "Common_factors_returns_Histograms.pdf", height = 3, width = 7)
Sub1 = subset(data, data$Company == levels(data$Company)[1])
Sub1 = Sub1[,8:11]
par(mfrow = c(1,4), mar = c(1,1,1,1), cex = 1, lwd = 2)
apply(Sub1, MARGIN = 2, FUN = histfun)
dev.off()
