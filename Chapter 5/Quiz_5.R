# Loading in the data:
load("5.R.RData")
attach(Xy)

# A simple linear fit. We see the standard error for
# the X1 slope comes out to 0.02593.
fit1 = lm(y~X1+X2)
summary(fit1)

# Looking at the data, however, we see that it has
# significant autocorrelation, meaning that we are
# likely overestimating the effective sample size
# and therefore our ability to estimate accuracy
# of our parameter estimates.
matplot(Xy,type="l")
dev.copy(png,'./Output/Quiz_Data.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# Doing a simple bootstrap, we come to about the
# same value for the standard error:
library(boot)
b1func = function(data, index) {
  with(data[index,], coef(summary(lm(y~X1+X2)))["X1","Estimate"])
}
boot(Xy, b1func, 1000)

# We should instead do a block bootstrap to take
# into account the autocorrelation. Indeed we find
# a much larger estimate for the standard error:
tsboot(Xy, b1func, 1000, sim = "fixed", l = 100)

# That raises a question, though. Presumably if we
# apply normal bootstrap and block bootstrap to
# uncorrelated data, we should get the same results.
# Let's try:
U1 = runif(1000)
U2 = runif(1000)
v = 2*U1 - 3*U2 + 1 + rnorm(1000)
Uv = data.frame(U1, U2, v)

b1funcUv = function(data, index) {
  with(data[index,], coef(summary(lm(v~U1+U2)))["U1","Estimate"])
}
b2funcUv = function(data, index) {
  with(data[index,], coef(summary(lm(v~U1+U2)))["U2","Estimate"])
}

matplot(Uv, type="l")
dev.copy(png,'./Output/Quiz_GeneratedData.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# We now have uncorrelated datapoints, so we should
# be able to rely on a normal bootstrap:
boot(Uv, b1funcUv, 1000)

# We again get a very similar value to the one we get
# from our linear regression:
coef(summary(lm(v~U1+U2)))

# When we now apply a block bootstrap, we indeed get
# a similar value:
tsboot(Uv, b1funcUv, 1000, sim = "fixed", l = 100)
