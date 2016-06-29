# === Load data and install packages ===
load("./Data-Set/RegressionBase.RData")

# Install packages if not installed
libraries = c("sandwich","lmtest","foreign","plm","car","stargazer","xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# exclude electr. companies(3&8)
data = subset(data, data$Company!="CPEnergy" & data$Company!="PG&E_Corp")

# === Create unconditioned correlation matrix for variables ===
# Excluding variables that are not interesting
data2 = data[,c(3:10)]

# Create funtion for matrix
corrma = function(x){ 
  require(Hmisc) 
  x = as.matrix(x) 
  R = rcorr(x)$r 
  p = rcorr(x)$P 
  
  # Define notions for significance levels
  stars = ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  # Trunctuate the matrix that holds the correlations to two decimal
  R = format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  # Build a new matrix that includes the correlations with their apropriate stars 
  Rnew = matrix(paste(R, stars, sep=""), ncol=ncol(x)) 
  diag(Rnew) = paste(diag(R), " ", sep="") 
  rownames(Rnew) = colnames(x) 
  colnames(Rnew) = paste(colnames(x), "", sep="") 
  
  # Remove upper triangle
  Rnew = as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = FALSE)] = ""
  Rnew = as.data.frame(Rnew) 
  
  # Remove last column and return the matrix (which is now a data frame)
  Rnew = cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

# Apply function and get otput
corrma(data2)
print(xtable(corrma(data2)), type = "latex", size = "tiny", file = "./Quantlet 3 _ PanelDataAnalysis/corrma.txt")

# === Regression Analysis ===
# Declare data as panel data
data = pdata.frame(data, index = c("Company", "Date"), 
                   drop.index = FALSE, row.names = TRUE) 

# Classify date as date in R
data$Date = as.Date(data$Date, "%Y-%m-%d")

# Do regression with fixed effects model
fe = plm(Stock ~ NI + BVE.MCAP + D.MCAP+Oil+Gas+Market, 
         model = "within" ,data=data)
summary(fe)

# Output for LATEX
mat                    = summary(fe)$coefficients
mat                    = mat[, c(1, 4)]
signif                 = rep("", dim(mat)[1])

signif[mat[, 2] < 0.1]  = "*"
signif[mat[, 2] < 0.05]  = "**"
signif[mat[, 2] < 0.01] = "***"

mat = as.data.frame(mat)

mat[, 2] = signif

names(mat) = c("Estimate", "")

mat[, 1] = round(mat[, 1], 2)


mat = cbind(mat)

xtable(mat)
print(xtable(mat), type = "latex", size = "tiny", file = "./Quantlet 3 _ PanelDataAnalysis/femodel.txt")

# Do regression with random effects model
re = plm(Stock ~ NI + BVE.MCAP + D.MCAP+Oil+Gas+Market, 
         model = "random",data=data)
summary(re)

# Output for LATEX
mat                    = summary(re)$coefficients
mat                    = mat[, c(1, 4)]
signif                 = rep("", dim(mat)[1])

signif[mat[, 2] < 0.1]  = "*"
signif[mat[, 2] < 0.05]  = "**"
signif[mat[, 2] < 0.01] = "***"

mat = as.data.frame(mat)

mat[, 2] = signif

names(mat) = c("Estimate", "")

mat[, 1] = round(mat[, 1], 2)


mat = cbind(mat)

xtable(mat)
print(xtable(mat), type = "latex", size = "tiny", file = "./Quantlet 3 _ PanelDataAnalysis/remodel.txt")

# === Statistical Tests & robust estimators ===
# Hausman test comparing RE and FE
phtest(fe, re)

# Test for serial correlation
pbgtest(re)
pdwtest(re)

# Test for heteroskedasticity
bptest(Stock ~ NI + BVE.MCAP + D.MCAP+Oil+Gas+Market+
       factor(Company), data=data, studentize=F)

# Heteroskedasticity and serial correlation consistent coefficient
arellano = coeftest(re, vcovHC(re,method="arellano"))
arellano

# Output for LATEX
mat                    = arellano[, c(1, 4)]
signif                 = rep("", dim(mat)[1])

signif[mat[, 2] < 0.1]  = "*"
signif[mat[, 2] < 0.05]  = "**"
signif[mat[, 2] < 0.01] = "***"

mat = as.data.frame(mat)

mat[, 2] = signif

names(mat) = c("Estimate", "")

mat[, 1] = round(mat[, 1], 2)


mat = cbind(mat)

xtable(mat)
print(xtable(mat), type = "latex", size = "tiny", file = "./Quantlet 3 _ PanelDataAnalysis/arellanomodel.txt")

# Cross-sectional dependence
pcdtest(fe, test = c("lm"))

# Cross-sectional robust (Driscoll and Kraay)
DriscollandKray = coeftest(re, vcov=vcovSCC)
DriscollandKray

# Output for LATEX
mat                    = DriscollandKray[, c(1, 4)]
signif                 = rep("", dim(mat)[1])

signif[mat[, 2] < 0.1]  = "*"
signif[mat[, 2] < 0.05]  = "**"
signif[mat[, 2] < 0.01] = "***"

mat = as.data.frame(mat)

mat[, 2] = signif

names(mat) = c("Estimate", "")

mat[, 1] = round(mat[, 1], 2)


mat = cbind(mat)

xtable(mat)
print(xtable(mat), type = "latex", size = "tiny", file = "./Quantlet 3 _ PanelDataAnalysis/DriscollandKraymodel.txt")
# GLS regression
repggls = pggls(Stock ~ NI + BVE.MCAP + D.MCAP+Oil+Gas+Market+ Oil+Gas, 
                model = "pooling", data=data)
summary(repggls)

# Output for LATEX
mat                    = summary(repggls)$CoefTable
mat                    = mat[, c(1, 4)]
signif                 = rep("", dim(mat)[1])

signif[mat[, 2] < 0.1]  = "*"
signif[mat[, 2] < 0.05]  = "**"
signif[mat[, 2] < 0.01] = "***"

mat = as.data.frame(mat)

mat[, 2] = signif

names(mat) = c("Estimate", "")

mat[, 1] = round(mat[, 1], 2)


mat = cbind(mat)
xtable(mat)
print(xtable(mat), type = "latex", size = "tiny", file = "./Quantlet 3 _ PanelDataAnalysis/repggls.txt")

# Pooltest
pooltest(Stock~NI + BVE.MCAP + D.MCAP+Oil+Gas+Market+ Oil+Gas,data=data,
        model="within")

# Unobserved effects test
pwtest(Stock~NI + BVE.MCAP + D.MCAP+Oil+Gas+Market+ Oil+Gas,data=data)
