# === Clearing the Environment ===
# remove variables
rm(list = ls())

# reset graphics
graphics.off()


# === Packages ===
# Install packages if not installed
libraries = c("plyr","dplyr","data.table", "tseries", "xtable", 
              "plm", "CADFtest")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
#  Load packages 
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
rm(libraries) # package loading done - not needed anymore

# === Define required functions ===
# Functions for variable transformatin later on
logreturnsfun = function(x){
  n = length(x)
  logreturn = diff(log(x), lag=1)
  # AS numeric part added
}

scalefun = function(x){
  ScaleVar = scale(x)
}


# Function for seperating the common factor developments apart from the others
ComFSep = function(x){
  if(class(x) == "data.frame"){
    Sub1 = subset(x, x$Company == levels(x$Company)[1])
    Sub1 = Sub1[,c("Oil", "Gas", "Market", "EURUSD")]
    # print("continue programming")
    return(Sub1)
  }else{
    print("Data must be passed as data frame or a subsection of it")
  }
}


# ======= Initial data modifications =====
# loading data
data = read.csv2("./Data-Set/Dataset-FINALupdated_absolute_2.csv", 
                 stringsAsFactors = FALSE)

# Convert Date such that R recognizes it as date
class(data$Date)
data$Date <- as.Date(data$Date, format = "%d.%m.%Y")
class(data$Date)

# Order data adequately for subsequent transformations
data = data[order(data$Company, data$Date),]


# Set shorter ColumnNames for subsequent analysis
colnames(data) = c("Date", "Company", "Stock", "A-MCAP", 
                   "BVE-MCAP", "D-MCAP", "NI", colnames(data[8:ncol(data)])) 

# Attach companyNames to the data set
data$Company = as.factor(data$Company) # Company variable from integer to factor
levels(data$Company) = c("Exxon_Mobil", "Apache", 
                         "CPEnergy", "Chevron", "Hess_Corp", "Murphy_Oil", 
                         "Occidental_Petroleum", "PG&E_Corp", "Williams")

# Save the dataset - used by graphics quantlet
save(data, file="./Data-Set/InitialData_Panel_Date_OK_OLD_ZScore.RData")


# === Apply (Stationarity) Transformations ==
# Apply log - transformation to variable
dataHelp = cbind(data[,1:2], log(data[, c(4)]), data[,c(18)])
colnames(dataHelp) = colnames(data[,c(1:2, 4, 18)])
# else:  "A-MCAP","D-MCAP"

# Apply log-return transformation to predefinded variable set
LogR = apply (
  X = data[,c(3,6, 8:17)], 
  MARGIN = 2 , 
  logreturnsfun
)

# Apply z-score transformation to company-specific variables
# NI and A-MCAP
ScaleD = aggregate(data[,c(5,7)], by = list(data$Company), 
                   simplify = FALSE, FUN = scale)
A = unlist(ScaleD$NI)
A = as.matrix(cbind(A,unlist(ScaleD$`BVE-MCAP`)))
colnames(A) = colnames(data[,c(7,5)])


#deleting false log return#
Datatrans = data.frame(cbind(dataHelp[-1,],LogR, A[-1,]))
Datatrans2 = subset(Datatrans, as.Date("1996-06-30")<Datatrans$Date)
#checking wich row have been deleted
H = anti_join(Datatrans,Datatrans2)
H$Date == "1996-06-28"
rm(H)
# The correct date, for which no log return can be obtained was deleted

# Final adjustments
dataFinal = Datatrans2[,c(1,2,4,17,6, 3, 18, 7,8,9:16,5)] # order data as before
# colnames(dataFinal) = colnames(data) # assign previous column names
data = dataFinal 
# remove auxiliary variables / data frames
rm(Datatrans2, Datatrans, LogR, ScaleD, 
   dataHelp, A, dataFinal)

# Generate Market excess return
data$Market = data$Market - data$T.Bill3M
# Remove 3 Month T-Bill Rate
data$T.Bill3M = NULL

# Store transformed dataset for regression analysis and graphical analysis
save(data, file="./Quantlet 5/Trans2TIMD/DebtReturn.RData")
