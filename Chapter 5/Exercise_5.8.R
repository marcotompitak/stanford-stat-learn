# Generating the data:
set.seed(1)
y = rnorm(100) # This seems useless, keeping it for
               # consistency with the exercise.
x = rnorm(100)
y = x - 2*x^2 + rnorm(100)
plot(x,y)
dev.copy(png,'./Output/Ex8_Data.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()
XY = data.frame(x,y)

# LOOCV test error as a function of polynomial degree:
err = rep(0, 4)
set.seed(1)
for(i in 1:4) {
  fit = glm(y~poly(x, i), data=XY)
  err[i] = cv.glm(XY, fit)$delta[1]
}
plot(err, type="l")
dev.copy(png,'./Output/Ex8_PolyError.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# We see that the accuracy increases going from order 1
# to order 2, but then there are no further reductions
# in error, as expected.

# If we set a different seed we get the same results, 
# because there is no randomness involved in LOOCV:
err2 = rep(0, 4)
set.seed(100)
for(i in 1:4) {
  fit = glm(y~poly(x, i), data=XY)
  err2[i] = cv.glm(XY, fit)$delta[1]
}
plot(err, err2, type="l")
abline(0,1,col="red", lty=3)
dev.copy(png,'./Output/Ex8_NoSeedEffect.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# If we consider the statistical significance of the
# polynomial coefficients, we see that indeed also there
# only the first two order terms are significant:
coef(summary(fit))
