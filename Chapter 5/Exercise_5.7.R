# Loading the data
library(ISLR)
attach(Weekly)

# LOOCV using the boot package:
library(boot)

# We need a custom cost function to perform classification:
classcost = function(r, pi = 0) mean(abs(r-pi) > 0.5)

# Fitting the model
fit = glm(Direction~Lag1+Lag2, family="binomial")

# Using cv.glm to perform LOOCV:
cv.glm(Weekly, fit, classcost)$delta


# Manual LOOCV yields the same result:
misclass = rep(FALSE, nrow(Weekly))
for(i in 1:nrow(Weekly)) {
  fit = glm(Direction~Lag1+Lag2, data=Weekly[-i,], family="binomial")
  pred = predict.glm(fit, Weekly[i,], type="response")
  clas = if ( pred > 0.5 ) "Down" else "Up"
  misclass[i] = clas == Weekly[i,"Direction"]
}
mean(misclass)
