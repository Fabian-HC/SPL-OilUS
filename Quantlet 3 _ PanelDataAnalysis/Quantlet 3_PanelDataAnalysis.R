

load("./Data-Set/For_Marcus_OK_Old_Version.RData")

# Install packages if not installed
libraries = c("sandwich","lmtest","foreign","plm","car","stargazer","xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# exclude electr. companies(3&8)
data = subset(Datatrans, Datatrans$Company!="CPEnergy" & Datatrans$Company!="PG&E_Corp")
remove(Datatrans)

#correlation coefficients
data2<-Datatrans[,c(3:10)]

corrma = function(x){ 
  require(Hmisc) 
  x = as.matrix(x) 
  R = rcorr(x)$r 
  p = rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  stars = ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R = format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew = matrix(paste(R, stars, sep=""), ncol=ncol(x)) 
  diag(Rnew) = paste(diag(R), " ", sep="") 
  rownames(Rnew) = colnames(x) 
  colnames(Rnew) = paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew = as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = FALSE)] = ""
  Rnew = as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew = cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}
corrma(data2)
xtable(corrma(data2))


# panel data now

data = pdata.frame(data, index = c("Company", "Date"), drop.index = FALSE, row.names = TRUE) 
data$Date = as.Date(data$Date, "%Y-%m-%d")

# fixed effects model
fe = plm(Stock ~ A.MCAP +NI + BVE.MCAP + D.MCAP+Oil+Gas+Market, 
         model = "within", data=data)
summary(fe)
stargazer(fe,title="Oneway (individual) effect Within Model",dep.var.labels=c("Stock return"),font.size = "tiny", out="./Regression-Analysis/femodel.LATEX")

# random effects model
re = plm(Stock ~ A.MCAP +NI + BVE.MCAP + D.MCAP+Oil+Gas+Market, 
         model = "random", data=data)
summary(re)
stargazer(re,title="Oneway (individual) effect Random Effect Model (Swamy-Arora's transformation)",
          dep.var.labels=c("Stock return"),font.size = "tiny",out="./Regression-Analysis/remodel.LATEX")

# Hausman test comparing RE and FE
phtest(fe, re)

# test for serial correlation
pbgtest(re)
pdwtest(re)

# test for heteroskedasticity
bptest(Stock ~ A.MCAP +NI + BVE.MCAP + D.MCAP+Oil+Gas+Market+
       factor(Company), data=data, studentize=F)

# heterosk. and serial corr. consistent coefficient
arellano = coeftest(re, vcovHC(re,method="arellano"))
arellano
# output
stargazer(arellano,title="Arellano model",dep.var.labels=c("Stock return"), font.size = "tiny", out="./Regression-Analysis/arellanomodel.LATEX")

# cross-sectional dependence
pcdtest(fe, test = c("lm"))

# cross-sectional robust (Driscoll and Kraay)
DriscollandKray = coeftest(re, vcov=vcovSCC)
DriscollandKray
# output
stargazer(DriscollandKray,title="cross-sectional robust",dep.var.labels=c("Stock return"), font.size="tiny",out="./Regression-Analysis/DriscollandKray.LATEX")


# GLS
repggls = pggls(Stock ~ A.MCAP +NI + BVE.MCAP + D.MCAP+Oil+Gas+Market+ Oil+Gas, 
                model = "pooling", data=data)
summary(repggls)

# Extract information manually and copy into report / slides
SumGLS$call
SumTabGLS = as.data.frame(SumGLS$CoefTable)
SumTabGLS2 = as.data.frame(cbind("Multiple R Square", SumGLS$rsqr, "Residual Sum SQ", SumGLS$ssr))
print(xtable(SumTabGLS), type = "latex", size = "tiny", file = "./Regression-Analysis/PPGLS-Result.txt")

#pooltest

pooltest(Stock~A.MCAP +NI + BVE.MCAP + D.MCAP+Oil+Gas+Market+ Oil+Gas,data=data,
        model="within")

#two-ways test

pFtest(Stock~A.MCAP +NI + BVE.MCAP + D.MCAP+Oil+Gas+Market+ Oil+Gas,data=data,
       effect="twoways")

#unobserved effects test

pwtest(A.MCAP +NI + BVE.MCAP + D.MCAP+Oil+Gas+Market+ Oil+Gas,data=data)





########### old code with csv file ###########

setwd("C:/Users/marcu/Downloads")

data <- read.csv2("C:/Users/marcu/Desktop/R/Dataset FINALFORPRESENTATION.csv", stringsAsFactors=FALSE)

# Install packages if not installed
libraries = c("sandwich","lmtest","foreign","plm","car","stargazer")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# exclude electr. companies(3&8)
data <- subset(data, data$Company!=3 & data$Company!=8)

# panel data now
data$Date<-as.Date(as.character(data$Date), "%d.%m.%Y")

data <- pdata.frame(data, index = c("Company", "Date"), drop.index = F, row.names = T) 
data$Date<-as.Date(as.character(data$Date), "%Y-%m-%d")

# fixed effects model
fe <- plm(Stock ~ A/MCAP +NI + BV(EQ)/MCAP + D/MCAP+Oil+Gas+Market, 
          model = "within", data=data)
summary(fe)
stargazer(fe,title="Oneway (individual) effect Within Model",dep.var.labels=c("Stock return"),
          covariate.labels=c("Assets over market cap.","Net income","Book value equity over market cap.",
          "Debt over equity","Oil price","Gas price","DJI premium"), out="femodel.LATEX")

# random effects model
re <- plm(Stock ~ A/MCAP +NI + BV(EQ)/MCAP + D/MCAP+Oil+Gas+Market, 
          model = "random", data=data)
summary(re)
res.2 <- re$residuals
stargazer(fe,title="Oneway (individual) effect Random Effect Model (Swamy-Arora's transformation)",
          dep.var.labels=c("Stock return"),covariate.labels=c("Assets over market cap.","Net income",
          "Book value equity over market cap.","Debt over equity","Oil price","Gas price","DJI premium"), 
          out="remodel.LATEX")

# Hausman test comparing RE and FE
phtest(fe, re)

# test for serial correlation
pbgtest(re)
pdwtest(re)

# test for heteroskedasticity
bptest(Stock ~ A/MCAP +NI + BV(EQ)/MCAP + D/MCAP+Oil+Gas+Market+
      factor(Company), data=data, studentize=F)

# heterosk. and serial corr. consistent coefficient
arellano<-coeftest(re, vcovHC(re,method="arellano"))
arellano
# output
stargazer(arellano,title="Arellano model",dep.var.labels=c("Stock return"),
          covariate.labels=c("Assets over market cap.","Net income","Book value equity over market cap.",
          "Debt over equity","Oil price","Gas price","DJI premium"), out="arellanomodel.LATEX")

# cross-sectional dependence
pcdtest(fe, test = c("lm"))

# cross-sectional robust (Driscoll and Kraay)
DriscollandKray<-coeftest(re, vcov=vcovSCC)
DriscollandKray
# output
stargazer(DriscollandKray,title="cross-sectional robust",dep.var.labels=c("Stock return"),
          covariate.labels=c("Assets over market cap.","Net income","Book value equity over market cap.",
          "Debt over equity","Oil price","Gas price","DJI premium"), out="DriscollandKray.LATEX")

# GLS
repggls <- pggls(Stock ~ A/MCAP +NI + BV(EQ)/MCAP + D/MCAP +Market+ Oil+Gas, 
                model = "pooling", data=data)
summary(repggls)
