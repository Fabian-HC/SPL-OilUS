library(xtable)

data2<-data[,c(3:11)]

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
xtable(corstarsl(data2))

