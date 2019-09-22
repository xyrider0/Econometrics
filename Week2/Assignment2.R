# Package dependencies
library("xlsx")

# Read Data
data <- read.xlsx("TestExer2-GPA-round2.xls", 1)

## Data Summary
# Observations = 609
# FGPA: Freshman grade point average (scale 0-4)
# SATV: Score on SAT Verbal test (scale 0-10)
# SATM: Score on SAT Mathematics test (scale 0-10)
# FEM: Gender dummy (1 for females, 0 for males)

# part a 
# (i)
# Regress FGPA on a constant and SATV
fit <- lm(FGPA ~ SATV, data=data)
# Report Coefficient of SATV and its standard error and p-value
# Within 3 decimals
coef <- summary(fit)$coef
SATV <- coef["SATV", colnames(coef[, c(1, 2, 4)])]
print("SATV Coefficients:")
print(round(SATV, 3))
# Sanity check
plot(data$SATV, data$FGPA, main="FGPA vs SATV", xlab="SATV", ylab="FGPA")
abline(fit)

# (ii)
# Determine a 95% confidence interval (with 3 decimals) for
# the effect on FGPA of an increase by 1 point in SATV
min95 <- round(SATV[1] - 2 * SATV[2],6)
max95 <- round(SATV[1] + 2 * SATV[2],6)
print("95% Confidence Interval of effect on FGPA by 1 point increase in SATV:")
print(c(min95, max95))

# part b
# Regress FGPA on a constant, SATV, SATM, and FEM
fit_multi <- lm(FGPA ~ SATV + SATM + FEM, data=data)
# Report Coefficient of SATV and its standard error and p-value
# Within 3 decimals
coef_multi <- summary(fit_multi)$coef
coef_ans <- coef_multi[c("(Intercept)","SATV", "SATM", "FEM"), colnames(coef_multi[, c(1, 2, 4)])]
print("Coefficients:")
print(round(coef_ans, 6))

# (ii)
# Determine a 95% confidence interval (with 3 decimals) for
# the effect on FGPA of an increase by 1 point in SATV
min95_multi <- sum(round(coef_ans[2] - 2 * coef_ans[2,2],6))
max95_multi <- sum(round(coef_ans[2] + 2 * coef_ans[2,2],6))
print("95% Confidence Interval of effect on FGPA by 1 point increase in SATV:")
print(c(min95_multi, max95_multi))

# part c
# Correlation Matrix
print("Correlation Matrix")
corr <- round(cor(data[,c(2,3,4,5)]),6)
print(corr)

# part d
# Perform an F-test on the significance of the effect of SATV on FGPA, based on the 
# regression in part b and another regression

# Unrestricted model R^2
r1_sq <- summary(fit_multi)$r.squared
n <- nrow(data)


# Restricted model R^2
fit_restr <- lm(FGPA ~ SATM + FEM, data=data)
r0_sq <- summary(fit_restr)$r.squared
g <- 1
k <- 3

F_score <- ((r1_sq - r0_sq)/g)/((1-r1_sq)/(n-k))
print("F Score")
print(F_score)

if(F_score > 3.9){
  print("SATV is significant")
}else{
  print("SATV is not significant")
}
