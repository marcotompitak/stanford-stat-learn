# Loading the data: the ISLR package contains the Carseats data
library(ISLR)
attach(Carseats)

# Fitting a multiple linear regression model to Sales predicted
# by Price, Urban and US. 
fit1 = lm(Sales~Price+Urban+US)
summary(fit1)

# We see that Urban has no significant effect, so we can remove it:
fit2 = update(fit1,Sales~.-Urban)
summary(fit2)
# We see that this has no effect on the fit (R-squared is 0.2393 in
# both models), so indeed Urban has no predictive value.

# 95% confidence intervals:
confint(fit2)

# Diagnostic plots show no big outliers, but some points with leverage
# a factor of a few larger than the average:
par(mfrow=c(2,2))
plot(fit2)