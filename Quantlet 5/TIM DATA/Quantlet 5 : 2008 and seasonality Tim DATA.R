

# remove variables
rm(list = ls())

# reset graphics
graphics.off()

getwd()

#load variable
load("./Data-Set/For_Marcus_OK_Old_Version.RData")
#data2 = read.csv2("./Data-Set/Dataset-FINALFORPRESENTATION.csv",stringsAsFactors = FALSE)
#class(data2$Date)
#data$Date <- as.Date(data2$Date, format = "%d.%m.%Y")
#class(data2$Date)

data[78,]
data2[78,]
class(data$Date)
#!(data$Company==3 | data2$Company==8),]
data0 = data[!(data$Company=="CPEnergy" | data$Company=="PG&E_Corp"),]
#!(data$Company=="CPEnergy" | data$Company=="PG&E_Corp"),]
class(data$Company)
#names(data0)[4:7] = c("A.MCAP", "BVE.MCAP", "D.MCAP","NET")
# Install packages if not installed
libraries = c("stargazer","tseries","plm")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

########### BEFOR / AFTER 2008 ############

# subsetting #
Pre2008 = pdata.frame(subset(data0, Date <= '2008-08-31'), 
                      index = c("Company", "Date"), 
                      drop.index = F, 
                      row.names = T) 
Post2008 = pdata.frame(subset(data0, Date > '2008-08-31'), 
                       index = c("Company", "Date"), 
                       drop.index = F, 
                       row.names = T) 

#two regressions
#pre 2008
repre2008 = plm(Stock ~ A.MCAP+ NI+ BVE.MCAP
                + D.MCAP+ Oil + Gas + Market + EURUSD, 
                model = "random", 
                data=Pre2008)
summary(repre2008)

#post 2008  
rePost2008 = plm(Stock ~ A.MCAP+ NI+ BVE.MCAP
                 + D.MCAP+ Oil + Gas + Market + EURUSD, 
                 model = "random", 
                 data=Post2008)
summary(rePost2008)

#results
stargazer(repre2008, rePost2008, type="text", 
          title="Oneway (individual) Random Effect Pre 2008 and post 2008",
          dep.var.labels=c("Stock return"),
          covariate.labels=c("Assets/market cap.","Net income","BVE/market cap.",
                             "Debt/equity","Oil price","Gas price", "DJI premium","EUR/Dollar"), 
          out="./Quantlet 5/Regression-2008.txt")

#using dummy varibales

#creating dummy#

DumP = as.numeric(data0$Date > '2008-08-31') 
length(DumP)
dataP = cbind(data0, "After2008(=1)" = DumP)
dataP = pdata.frame(dataP, index = c("Company", "Date"), drop.index = F, 
                    row.names = T) 

#Random Effect with Dummy => DumP*D.MCAP


DumyReg = plm(Stock ~ A.MCAP+ NI + BVE.MCAP
              + D.MCAP + Oil + Gas + Market + EURUSD 
              + DumP*(A.MCAP) + DumP*NI + DumP*(BVE.MCAP)  + #DumP*D.MCAP#
              + DumP*Oil + DumP*Gas + DumP*Market + DumP*EURUSD, 
              model = "random", 
              data=dataP)
summary(DumyReg)


stargazer(DumyReg,type="text",
          title="Random Effect Model with Dummy after 2008 (= 1)",
          dep.var.labels=c("Stock return"),
          # covariate.labels=c("Assets/market cap.","Net income","BVE/market cap.",
          "Debt/equity","Oil price","Gas price","DJI premium","EUR/Dollar",
          "Dummy","Assets/market cap*Dummy","Net income*Dummy","BVE/market cap*Dummy",
          "Debt/equity*Dummy","Oil price*Dummy","Gas price*Dummy","DJI premium*Dummy","EUR/Dollar*Dummy"), 
out="./Quantlet 5/Regression-Dummy2008.txt")

###############  SEASONALITY  #############

###creating factor###
#Function#
quarters = lapply(data0$Date,quarters)
quarter2 = unlist(quarters)

#Data#
dataQ = cbind(data0,"Quarter" = quarter2)
dataQ[,"Quarter"] = factor(dataQ[,"Quarter"])
is.factor( dataQ[,"Quarter"])
dataQ = pdata.frame(dataQ, index = c("Company", "Date"), 
                    drop.index = F, 
                    row.names = T) 

#regression#
Quarter = plm(Stock ~ A.MCAP+NI+ BVE.MCAP + D.MCAP+ Oil + Gas + Market + EURUSD + Quarter, 
              model = "random",
              data=dataQ)
stargazer(Quarter,type="text",
          title="Random Effect Model with Quarter Demmy",
          dep.var.labels=c("Stock return"),
          covariate.labels=c("Assets/market cap.","Net income","BVE/market cap.",
                             "Debt/equity","Oil price","Gas price", "DJI premium","EUR/Dollar"), 
          out="./Quantlet 5/QuarterDummy.txt")


###############  Type of Firm  #############

## creation of a dummy such as Oil firm = 0, Other = 1##
## Carefull use of data with all the firm ##

# subsetting #
# the first subset has already been created #
OilC = data0
OtherC = pdata.frame(subset(data,data$Company == "CPEnergy" | data$Company == "PG&E_Corp"),
                     index = c("Company", "Date"), 
                     drop.index = F, 
                     row.names = T) 
#Random Effect#
#OilC
reOilC = plm(Stock ~ A.MCAP+ NI+ BVE.MCAP
             + D.MCAP+ Oil + Gas + Market + EURUSD, 
             model = "random", 
             data=OilC)
summary(reOilC)

#OtherC  
reOtherC = plm(Stock ~ A.MCAP+ NI+ BVE.MCAP
               + D.MCAP+ Oil + Gas + Market + EURUSD, 
               model = "random", 
               data=OtherC)
summary(reOtherC)

#results
stargazer(reOilC, reOtherC, type="text", 
          title="Random Effect Model depending on Company type",
          dep.var.labels=c("Stock return"),
          covariate.labels=c("Assets/market cap.","Net income","BVE/market cap.",
                             "Debt/equity","Oil price","Gas price", "DJI premium","EUR/Dollar"), 
          out="./Quantlet 5/Regression.Oil-Other.txt")

############### Regression with a Dummy ################

DumFirmT = ifelse(data$Company == "CPEnergy" | data$Company == "PG&E_Corp" ,1,0)
length(DumFirmT)
DataT = cbind(data, DumFirmT)
DataT = pdata.frame(DataT, index = c("Company", "Date"), 
                    drop.index = F, 
                    row.names = T) 


TypeFirm = plm(Stock ~ A.MCAP+NI+ BVE.MCAP
               + D.MCAP+ Oil + Gas + Market + EURUSD + DumFirmT + DumFirmT*NI+ DumFirmT*BVE.MCAP
               + DumFirmT*D.MCAP+ DumFirmT*Oil + DumFirmT*Gas + DumFirmT*Market + DumFirmT*EURUSD, 
               model = "random",
               data=DataT)
stargazer(TypeFirm,type="text",
          title="Random Effect Model with Dummy for Type of firm",
          dep.var.labels=c("Stock return"),
          covariate.labels=c("Assets/market cap.","Net income","BVE/market cap.",
                             "Debt/equity","Oil price","Gas price","EUR/Dollar",
                             "Dummy","Assets/market cap*Dummy","Net income*Dummy","BVE/market cap*Dummy",
                             "Debt/equity*Dummy","Oil price*Dummy","Gas price*Dummy","EUR/Dollar*Dummy"), 
          out="./Quantlet 5/Regression.DummyOil-Other.txt")








