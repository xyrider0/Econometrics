library("xlsx")
data <- read.xlsx("TrainExer13.xls", 1)

#a - Use all data to estimate coefficients a and b in simple regression line
fit <- lm(Expenditures ~ Age, data=data)
summary(fit)

#b Plot best fit line
plot(Expenditures ~ Age, data=data, main='regression of TestExer1')
abline(fit)

#c Split data into two sets of observations
u40data <- data[data['Age']<40,]
a40data <- data[data['Age']>=40,]

u40fit <- lm(Expenditures ~ Age, data=u40data)
a40fit <- lm(Expenditures ~ Age, data=a40data)
summary(u40fit)
summary(a40fit)

#Discuss main differences between parts a and c
plot(Expenditures ~ Age, data=data, main='regression of under and over Age 40 TestExer1')
abline(u40fit)
abline(a40fit)
