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
  # plot(x, type = "h", ) Try this one to include y axis limits
  # ylim missing
  # hist(x, freq = FALSE, main = NULL, xaxp = NULL, yaxp = NULL)
  hist(x, freq = FALSE, main = NULL, yaxt = 'n', xaxt = 'n')
  lines(g1, dens1, col = "red", lwd = 10)
}

plotfun = function(x){

  Name = colnames(x) # we cannot extract the column name in aggregate and neither
  # in apply - that is why we decided to still resort to the loop
  YMinMax = c(min(x)*0.9, max(x)*1.1)
  p = plot(x, type = "l", xaxt = 'n', xlab = "", ylab = "", ylim = YMinMax)
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
setwd("C:/Users/Trimme/Documents/GitHub/R_Project/SPL-OilUS/Graphs_Final")

# Load the RData Set
load("C:/Users/Trimme/Documents/GitHub/R_Project/SPL-OilUS/Data-Set/InitialData_Date_OK.RData")


# Obtain all stock performances and Index them
# Base: 1996-06-28 = 100%
StockSet = data.frame(t(as.matrix(aggregate(data$Stock, by = list(data$Company), FUN = indexFUN, simplify = TRUE))))
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
# StockSet = StockSet[-1,]

# On some occasions, the previous transformations yielded the correct values
# , but provided them as character variables. Just in case that this still happens
# 
if(class(StockSet[,1]) != "numeric"){
  StockSet = data.frame(apply(StockSet, MARGIN = 2, FUN = as.numeric))
}else{
  print("Indexed stock performance data is already numeric")
}

# Save the stock set data
save(data, file="~/GitHub/R_Project/SPL-OilUS/Data-Set/Stock_Set_Indexed.RData")

# remove the first row because it contains the company number
StockSet = StockSet[-1,]
# Add the date vector to our Stock Set to get a time axis later on
DateVec = subset(data$Date, data$Company == 1)
# The same story with ggplot
# DateVec = sort(DateVec)
StockSet = cbind(DateVec, StockSet)
# Set the variable Names
StockNames = c("Exxon", "Apache", "C. Point", "Chevron", "Hess", "Murphy Oil", "Occidental", "PG&E", "Williams")
colnames(StockSet) = c("Date", StockNames)
# Preparing Plot - Using GGPLOT
VarSetMelt <- melt(StockSet, id = "Date")
# VarSetMelt[VarSetMelt$variable == "X1"] = "Exxon"
# Plot the Indexed Stock Prices Together
dev.off() # necessary before plotting again
jpeg(filename = "All_Stocks_Plot.jpg")
# Plot the indexed stock prices
p <- ggplot(data=VarSetMelt, aes(x = VarSetMelt$Date, y=value, colour=variable)) + geom_line(size = 1.5)
p = p + theme(legend.position="bottom")
p = p + theme(legend.title=element_text(size = 0, colour = "white"))
p = p + theme(panel.background = element_rect(fill="white"), axis.line = element_line(colour = "black"))
p = p + xlab("Year") + ylab("Index 1996 = 100")
p = p + theme(text = element_text(size=17))
p = p + theme(plot.margin = unit(c(1, 1, 1, 1),"cm"))
# Adjust the colors
p = p + scale_color_manual(values=c("deeppink", "black", "darkgreen", "darkblue", "darkred", "darkgrey", "burlywood4", "darkmagenta", "darkgoldenrod1"  ))
# p = p + theme(panel.grid.major = element_line(colour = "black", size = 0.85))
 # p = p + theme(axis.line = element_line(colour = "black", size = 1))
p
dev.off()
# remove help variables used
rm(StockSet, VarSetMelt, StockNames, p, i, DateVec)
dev.off()

Sub1 = subset(data, data$Company == 1)
Sub1 = Sub1[,c(1,8:11)]
#  Plot Oil, Gas and Market beside one and another
jpeg(filename = "Common_Factors_Development.jpg", height = 400, width = 1000)
par(mfrow = c(1,4))
par(cex = 1.3)
apply(Sub1[,2:5], MARGIN = 2, FUN = plotfun)
  dev.off()

# Yet I hold that with Titles explaining what is what, 
# This looks way better
jpeg(filename = "Common_Factors_Development_better.jpg", height = 400, width = 1000)
VarNames = colnames(Sub1)
par(mfrow = c(1,4))
par(cex = 1.3)
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
jpeg(filename = "Companies_Specific and common factors.jpg", width = 3000, height = 1800)
par(mfcol = c(9,9))
par(mar = c(1,1,1,1))
par(cex = 1.3)

aggregate(data[,3:11], by = list(data$Company), FUN = plotfun, simplify = FALSE)
dev.off()

# Version with loops, but more presentable output
# If necessary, load data set with comapany numbers, instead of names
VarNames = colnames(data)
VarNames[4] = "Assets/MCAP"
VarNames[5] = "BV(EQ)/MACP"
VarNames[6] = "D / E"

i = 1
while(i < 10){
  jpeg(filename = paste("Company_", i, "_Specific_and_Common_Factors.jpg"), width = 2100, height = 300)
  par(mfrow = c(1,9))
  par(mar = c(2,2,2,2))
  par(cex = 1.3)
  Sub1 = subset(data, data$Company == i)
  j = 3
  while(j < 12){
    YMinMax = c(min(Sub1[,j])*0.9, max(Sub1[,j])*1.1)
    plot(Sub1$Date, Sub1[,j], type = "l", ylim = YMinMax, main = paste("C_", i, "_", VarNames[j], sep = ""), xlab = "", ylab = "")
    j = j + 1
  }
  dev.off()  
  i = i + 1 
}
dev.off()



# since we switch to another dataset, remove 
# all the stuff we stored so far
rm(i, j, libaries, VarNames, YMinMax, YaXp)

load("C:/Users/Trimme/Documents/GitHub/R_Project/SPL-OilUS/Data-Set/TransformedDate.RData")
data = dataFinal
rm(dataFinal)
MainText = variable.names(data)


# Generate Histograms for Specific Factors by enterprise
# Now without any loop!!!
jpeg(filename = "Companies_Specific_and_common_factors_Histograms.jpg", width = 3000, height = 1800)
par(mfcol = c(9,5))
par(mar = c(1,1,1,1))
par(cex = 1.3)
par(lwd = 5)
aggregate(data[,3:7], by = list(data$Company), FUN = histfun, simplify = FALSE)
dev.off()