# Generating data with linearly related predictors:
set.seed (1)
x1 = runif (100)
x2 = 0.5*x1 + rnorm(100)/10
y  = 2 + 2*x1 + 0.3*x2 + rnorm(100)

# The predictors are not independent:
par(mfrow=c(1,1))
plot(x1,x2)
dev.copy(png,'./Output/Ex14_CorrelatedPredictors.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# If we create a fit with both predictors, we see that
# only one is found to have a significant effect. The
# slope estimates are also not very accurate:
fit1 = lm(y~x1+x2)
summary(fit1)

# If we create fits with only one of the parameters, we
# see that either predictor is significantly predictive
# in the absence of the other one. This is as expected,
# since x1 and x2 are simply linearly related.
summary(lm(y~x1))
summary(lm(y~x2))


# We now add an artificial "erroneous" point:
x1_new = c(x1, 0.1)
x2_new = c(x2, 0.8)
y_new = c(y, 6)

# Considering the range of the parameters, this point is
# a bit of an outlier for x2, but not for x1 and y. 
range(x1)
range(x2)
range(y)

# This means that, when using x2 as a predictor, this
# new point will be a high-leverage point. Generating
# new fits, we see that indeed, the new point is high-
# leverage for the models y~x1+x2 and y~x2, but not
# for the model y~x1. The point seems like something
# of an outlier when fitting y~x1.
fit2 = lm(y_new~x1_new+x2_new)
fit3 = lm(y_new~x1_new)
fit4 = lm(y_new~x2_new)

par(mfrow=c(2,2))
plot(fit2)
dev.copy(png,'./Output/Ex14_Outlier_x1x2.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()
plot(fit3)
dev.copy(png,'./Output/Ex14_Outlier_x1.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()
plot(fit4)
dev.copy(png,'./Output/Ex14_Outlier_x2.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()


