# Generating the data:
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100)

# Linear regression without intercept:
fit1 = lm(y~x+0)
par(mfrow=c(1,1))
plot(x,y)
abline(fit1, col="red")
summary(fit1)

# Linear regression on the inverse:
fit2 = lm(x~y+0)
plot(y,x)
abline(fit2, col="red")
summary(fit2)

# We see that, even though the slope estimate differs (the first fit
# is much closer to the correct value 2 than the second is to 1/2),
# the t-statistic has the same value. It can by shown analytically that
# the expression for the t-statistic for a linear regression without
# intercept is symmetric in exchange of x and y, and the value will
# therefore always be identical.
#   We can show that the same still holds if we make a linear fit 
# including an intercept:

fit3 = lm(y~x)
sum3 = summary(fit3)
fit4 = lm(x~y)
sum4 = summary(fit4)
coef(sum3)[, "t value"]["x"]
coef(sum4)[, "t value"]["y"]

