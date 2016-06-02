install.packages("stargazer") 
library(stargazer)

stargazer(mydata, type="text",title="Descriptive statistics", out="table1.LATEX")
