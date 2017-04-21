# Generating the data:
sed.seed(1)
x = rnorm(100)
e = rnorm(100, sd = 0.5)
y = 0.5*x -1 + e

# Linear regression:
fit1 = lm(y~x)

# Generating the plot:
plot(x,y)
abline(-1, 0.5, col="red")
abline(fit1, col="blue")
legend("topright", legend=c("Population regression", "Linear fit"), col=c("red","blue"), lwd=1, text.width=2, y.intersp = 2)
dev.copy(png,'./Output/Ex13_LinFit.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# If we use a quadratic regression, we see that the second-order coefficient is estimated to be zero:
fit2 = lm(y~poly(x,2, raw = TRUE))
coef(summary(fit2))

# Repeating the linear fit with less noise:
sed.seed(1)
x = rnorm(100)
e = rnorm(100, sd = 0.05)
y = 0.5*x -1 + e
fit3 = lm(y~x)
plot(x,y)
abline(-1, 0.5, col="red")
abline(fit1, col="blue")
legend("topright", legend=c("Population regression", "Linear fit"), col=c("red","blue"), lwd=1, text.width=2, y.intersp = 2)
dev.copy(png,'./Output/Ex13_LinFit_LessNoise.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# And with more noise:
x = rnorm(100)
e = rnorm(100, sd = 5)
y = 0.5*x -1 + e
fit4 = lm(y~x)
plot(x,y)
abline(-1, 0.5, col="red")
abline(fit1, col="blue")
legend("topright", legend=c("Population regression", "Linear fit"), col=c("red","blue"), lwd=1, text.width=2, y.intersp = 2)
dev.copy(png,'./Output/Ex13_LinFit_MoreNoise.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# The noise level of course affects the estimation of the regression parameters:
confwidth1 = confint(fit1)["x",2] - confint(fit1)["x",1]
confwidth3 = confint(fit3)["x",2] - confint(fit3)["x",1]
confwidth4 = confint(fit4)["x",2] - confint(fit4)["x",1]
confwidths = c(confwidth3, confwidth1, confwidth4)
barplot(confwidths, xlab="Noise level", ylab = "Width of confidence interval on slope", names.arg = c(expression(paste(sigma, " = 0.05")), expression(paste(sigma, " = 0.5")), expression(paste(sigma, " = 5"))))
dev.copy(png,'./Output/Ex13_ConfWidths.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()
