# === Clearing the Environment ===
# remove variables
rm(list = ls())

# reset graphics
graphics.off()

# === Packages ===
# Install packages if not installed
libraries = c("stats", "graphics", "timeSeries", "reshape2", "ggplot2", 
              "psych", "xtable", "plyr","dplyr","data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
rm(libraries)


# === Define required functions ===
# Function for histograms of transformed variables
histfun = function(x){
  g1 = seq(min(x), max(x), length = 100)
  dens1 = dnorm(g1, mean(x), sd(x))
  maxHist = c(max(x), max(dens1))
  hist(x, freq = FALSE, main = NULL, yaxt = 'n', xaxt = 'n')
  lines(g1, dens1, col = "red", lwd = 2)
}

# Indexation function to compare stock performances
indexFUN = function(x){
  NObs = length(x)
  Index = x/x[1]*100
  Index = round(Index, digits = 2)
}


# ========================================================================
# SUMMARY STATISTICS SECTION
# ========================================================================

# === Summary Statistics of variables before transformation ===
# Load respective dataset
load("./Data-Set/InitialData_Panel.RData", verbose = FALSE)

# Summary statistics for common factors
Sub1 = subset(data, data$Company == levels(data$Company)[1])
SumCommonF = describe(Sub1[,8:ncol(data)], skew = TRUE, trim = 0, type = 1)

# Extract summary statistics of interest
SumCommonF = round(SumCommonF[,-c(1,2,5:7,10:13)], digits = 2)

# Export as CSV
write.csv2(SumCommonF, file = "./Quantlet2 _ EDA/Common_Factors_Absolute.csv")
# Export as TexFile
print.xtable(xtable(SumCommonF), file = "./Quantlet2 _ EDA/Common_Factors_Absolute.txt", size = "tiny")
rm(SumCommonF, Sub1) # remove auxiliary variables

# Summary statistics of company-specific variables
SumSpecF = describeBy(data[,2:7], group = "Company", 
                      mat = TRUE, digits = 2, trim = 0, type = 1)
# Extracting summary statistics of interest and cosmetic adjustments
SumSpecF = SumSpecF[-c(1:9),]
SumSpecF = SumSpecF[,-c(4,7:9,12:15)]
SumSpecF = SumSpecF[order(SumSpecF$group1, SumSpecF$vars),]
SumSpecF$vars = factor(SumSpecF$vars)
levels(SumSpecF$vars) = colnames(data[,3:7])
SumSpecF = SumSpecF[,-1]
rownames(SumSpecF) = NULL

# Export as CSV
write.csv2(SumSpecF, file = "./Quantlet2 _ EDA/Specific_Factors_Absolute.csv")
# Export as TexFile
print.xtable(xtable(SumSpecF), file = "./Quantlet2 _ EDA/Specific_Factors_Absolute.txt", size = "tiny")
rm(data, SumSpecF)

# === Summary Statistics of variables after transformations ===
# load the respective dataset
load("./Data-Set/RegressionBase.RData", verbose = TRUE)

# Summary statistics for common factors
Sub1 = subset(data, data$Company == levels(data$Company)[1])
SumCommonF = describe(Sub1[,8:ncol(data)], skew = TRUE, trim = 0, type = 1)
SumCommonF = round(SumCommonF[,-c(1,2,6,7,10,13)], digits = 2)
# Export as CSV
write.csv2(SumCommonF, file = "./Quantlet2 _ EDA/Common_Factors_returns.csv")
# Export as TexFile
print.xtable(xtable(SumCommonF), file = "./Quantlet2 _ EDA/Common_Factors_returns.txt", size = "tiny")
rm(Sub1, SumCommonF)

# Summary statistics of company-specific variables
SumSpecF = describeBy(data[,2:7], group = "Company", mat = TRUE, digits = 2, 
                      trim = 0, type = 1)
SumSpecF = SumSpecF[-c(1:9),]
SumSpecF = SumSpecF[,-c(4,8,9,12,15)]
SumSpecF = SumSpecF[order(SumSpecF$group1, SumSpecF$vars),]
SumSpecF$vars = factor(SumSpecF$vars)
class(SumSpecF$vars)
levels(SumSpecF$vars) = colnames(data[,3:7])
SumSpecF = SumSpecF[,-1]
rownames(SumSpecF) = NULL
write.csv2(SumSpecF, file = "./Quantlet2 _ EDA/Specific_Factors_returns.csv")
# Export as TexFile
print.xtable(xtable(SumSpecF), file = "./Quantlet2 _ EDA/Specific_Factors_returns.txt", size = "tiny")

# remove the remaining variables
rm(data, SumSpecF)





# ==============================================================
# GRAPHICAL ANALYSIS SECTION
# ==============================================================
# === Explorative Graphical Analyses of pre-transformed data ===
load("./Data-Set/InitialData_Panel.RData", 
     verbose = FALSE)


# === Generation of plots for presentation ===
class(data$Company)
levels(data$Company)
Sub1 = subset(data, data$Company == "Williams")

# Generate PDF recording and configurations
pdf(file = "./Quantlet2 _ EDA/C9_WilliamsPresentation.pdf", 
    height = 4, width = 10)
VarNames = colnames(Sub1)
par(mfcol = c(1,2))
par(cex = 1.5)
par(mar = c(2,2,1.5,1), lwd = 2)
plot(Sub1$Date, Sub1$Stock, type = "l", main = "Stock", 
     xlab = "", ylab = "", yaxt = "n")
axis(2, at=c(seq(0,60,20)) ,labels= TRUE , col.axis="black", las=2)
plot(Sub1$Date, Sub1$D.MCAP, type = "l", main = "D.MCAP", yaxt = "n")
axis(2, at = c(seq(100,300,100)), labels= TRUE, col.axis="black", las=2)
dev.off()


# === Generation of plot comparing stock performances ===
# Base: 1996-06-28 = 100%
# === Obtain all stock performances and Index them ===

StockSet = data.frame(t(as.matrix(
           aggregate(data$Stock, by = list(data$Company), 
           FUN = indexFUN, simplify = TRUE))))
colnames(StockSet) = levels(data$Company)
StockSet = StockSet[-1,]
# Check whether stock Index values are numeric
if(class(StockSet[,1]) != "numeric"){
  StockSet = data.frame(apply(StockSet, MARGIN = 2, FUN = as.numeric))
  print("StockSet converted to numeric")
}else{
  print("Indexed stock performance data is already numeric")
}


# Attach a date Vector to the Stock set
StockSet = cbind(data$Date[1:79], StockSet)
colnames(StockSet) = c("Date", colnames(StockSet[,-1]))

# Preparing plot data 
VarSetMelt <- melt(StockSet, id = "Date") # bring data into right format

# Plot the Stock set data
dev.off() # get rid of previous plot configurations

# Generate the plot
pdf(file = "./Quantlet2 _ EDA/All_Stocks_plot.pdf", height = 3, width = 5.25)
p <- ggplot(data=VarSetMelt, aes(x = VarSetMelt$Date, y=value, 
                                 colour=variable)) + geom_line(size = 0.75)
# Add legend configurations
p = p + theme(legend.position="bottom", 
              legend.title=element_text(size = 0, colour = "white"))
# Adjust panel background and axis lines
p = p + theme(panel.background = element_rect(fill="white"), 
              axis.line = element_line(colour = "black"))
# Assing axis labels
p = p + xlab("Year") + ylab("Index 1996 = 100")
# Change appearence of x and y axes
p = p + theme(axis.title.y = element_text(size = rel(1.1)), 
              axis.title.x = element_text(size = rel(1.1)), 
              axis.text = element_text(colour = "black", size = rel(1.2))) 
p = p + theme(text = element_text(size=9)) # Adjust text size
p = p + theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1),"cm")) # set plot margins
# Adjust the colors
p = p + scale_color_manual(values=c("deeppink", "black", "darkgreen", 
                                    "darkblue", "darkred", "darkgrey", 
                                    "burlywood4", "darkmagenta", "darkgoldenrod1"))
p
dev.off()
# Remove auxiliary variables used
rm(StockSet, VarSetMelt, p) # remove auxiliary variables
dev.off()


# === Plot common factors evolvement beside one another ===
#  prepare the data
Sub1 = subset(data, data$Company == levels(data$Company)[1])
Sub1 = Sub1[,c(1,8:11)]

# Generate PDF recording and configurations
pdf(file = "./Quantlet2 _ EDA/Common_Factors_Development_better.pdf", 
    height = 4,     width = 10)
VarNames = colnames(Sub1)
par(mfrow = c(1,4))
par(cex = 0.95)
par(mar = c(2,2,1.5,1), lwd = 2)

# Execute the loop for plotting the variables of interest 
i = 2
while(i < (ncol(Sub1) + 1)){
  YMinMax = c(min(Sub1[,i])*0.9, max(Sub1[,i])*1.1)
  plot(Sub1$Date, Sub1[,i], type = "l", ylim = YMinMax, main = VarNames[i], 
       xlab = "", ylab = "")
  i = i + 1
}
dev.off()
# remove auxiliary variables used
rm(VarNames, i, Sub1, YMinMax)


# === Plot of development of specific and common factors by enterprise ===
VarNames = colnames(data) # Store variable names for later purposes

# Execute the loop to generate the graphs
i = 1
while(i < 10){
  # generate the PDF containing the plots for company 'i'
  pdf(file = paste("./Quantlet2 _ EDA/Company_", 
      i, "_Specific_and_Common_Factors.pdf", sep = ""), 
      height = 2.35, width =11)
  
  # Plot conficurations
  par(mfrow = c(1,9), cex = 0.8, cex.main = 0.85, cex.axis = 0.7)
  # Extracting variable data for the respective company 'i'
  Sub1 = subset(data, data$Company == levels(data$Company)[i])
  
  j = 3 # Execute the loop over the variables of interest
  while(j < 12){
    if(j < 11){
      par(mar = c(2,2,2,0))
    }else{
      par(mar = c(2,2,2,0.7))
    }
    YMinMax = c(min(Sub1[,j])*0.9, max(Sub1[,j])*1.1)
    plot(Sub1$Date, Sub1[,j], type = "l", ylim = YMinMax, 
         main = paste(levels(data$Company)[i], "\n", VarNames[j], sep = ""), 
         xlab = "", ylab = "")
    j = j + 1
  }
  dev.off()  # end recording of the PDF
  i = i + 1 
}
dev.off() 

# Remove auxiliary variables
rm(i, j, VarNames, YMinMax, data, Sub1)


# === Histograms of transformed variables ===
# Load data
load(file="./Data-Set/RegressionBase.RData", verbose = TRUE)

# === Generate Histograms for Specific Factors by enterprise ===
# Set PDF rcording and configurations
pdf(file = "./Quantlet2 _ EDA/Companies_Specific_factors_returns_Histograms.pdf", 
    height = 6.1, width = 11)
par(mfcol = c(9,5))
par(mar = c(0.5,0.5,0.5,0.5))
par(cex = 1)
par(lwd = 2)

# Launch the aggregate function to perform histfun over variables of interest
# by company
aggregate(data[,3:7], by = list(data$Company), FUN = histfun, simplify = FALSE)
dev.off()


# === Generate histograms for common factors ===
pdf(file = "./Quantlet2 _ EDA/Common_factors_returns_Histograms.pdf", 
    height = 3, width = 7)
Sub1 = subset(data, data$Company == levels(data$Company)[1])
Sub1 = Sub1[,8:11]
par(mfrow = c(1,4), mar = c(1,1,1,1), cex = 1, lwd = 2)
apply(Sub1, MARGIN = 2, FUN = histfun)
dev.off()

rm(data, Sub1)
