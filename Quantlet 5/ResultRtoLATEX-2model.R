Beispiel in R (nicht styleguide-konform):

library(xtable)

# output
mat1                    = summary(reOilC)$coefficients
mat1                    = mat1[, c(1, 4)]
signif                 = rep("", dim(mat1)[1])

signif[mat1[, 2] < 0.10]  = "*"
signif[mat1[, 2] < 0.05]  = "**"
signif[mat1[, 2] < 0.01] = "***"

mat1 = as.data.frame(mat1)
mat1[, 2] = signif
names(mat1) = c("Estimate", "")
mat1[, 1] = round(mat1[, 1], 2)

mat2                    = summary(reOtherC)$coefficients
mat2                    = mat2[, c(1, 4)]
signif                 = rep("", dim(mat2)[1])

signif[mat2[, 2] < 0.10]  = "*"
signif[mat2[, 2] < 0.05]  = "**"
signif[mat2[, 2] < 0.01] = "***"

mat2 = as.data.frame(mat2)
mat2[, 2] = signif
names(mat2) = c("Estimate", "")
mat2[, 1] = round(mat2[, 1], 2)

#bind
mat3 = cbind(mat1, mat2)
xtable(mat3)
