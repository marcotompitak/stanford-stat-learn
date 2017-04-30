# Loading the data
library(ISLR)
attach(Default)

# Basic logistic regression:
fit1 = glm(default~income+balance, family = "binomial")
summary(fit1)

# We now want to perform cross-validation; we'll make it 10-fold.

# Separating the data into 10 random folds:
shufDefault = Default[sample(nrow(Default)),]
folds <- cut(seq(1,nrow(Default)),breaks=10,labels=FALSE)

# For each of the folds...
test_error = rep(0, 10)
for(i in 1:10) {
  # ...we choose one as test data...
  test_ind = which(folds==i)
  train = Default[-test_ind,]
  test = Default[test_ind,]
  truth = (test$default == "Yes")
  
  # ...fit to the training data...
  fit = glm(default~income+balance, data=train, family = "binomial")
  
  # ...predict the test data...
  pred = predict.glm(fit, newdata = test, type = "response") > 0.5
  
  # ...and calculate the error rate.
  test_error[i] = mean(pred!=truth)
}

# We now have a measure for the test error:
mean(test_error)
sd(test_error)

# Performing the same analysis using student as an additional variable
# does not change the test error significantly:
for(i in 1:10) {
  test_ind = which(folds==i)
  train = Default[-test_ind,]
  test = Default[test_ind,]
  truth = (test$default == "Yes")
  fit = glm(default~income+balance+student, data=train, family = "binomial")
  pred = predict.glm(fit, newdata = test, type = "response") > 0.5
  test_error[i] = mean(pred!=truth)
}
mean(test_error)
sd(test_error)