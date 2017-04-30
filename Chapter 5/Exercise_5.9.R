# Loading the data:
library(MASS)
attach(Boston)

# Estimating the mean of medv:
mean(medv)
# Standard error:
sd(medv)/sqrt(length(medv))

# Using bootstrap, the result is similar:
library(boot)
meanfunc = function(data, index) {
  with(data[index,], mean(medv))
}
meanboot = boot(Boston, meanfunc, 100)
meanboot

# Confidence interval for the mean. The results
# are similar using bootstrap or a t-test.
c(mean(medv) - 2*sd(meanboot$t), mean(medv) + 2*sd(meanboot$t))
t.test(medv)

# Now the median:
median(medv)
medfunc = function(data, index) {
  with(data[index,], median(medv))
}
medboot = boot(Boston, medfunc, 100)
sd(medboot$t)

# The tenth quantile:
quantile(medv, probs=0.1, names=FALSE)
q10func = function(data, index) {
  with(data[index,], quantile(medv, probs=0.1, names=FALSE))
}
q10boot = boot(Boston, q10func, 100)
sd(q10boot$t)

