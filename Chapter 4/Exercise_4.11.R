# Loading in the data
library(ISLR)
attach(Auto)

# Creating a new categorical value for mpg:
mpg01 = mpg > median(mpg)
Auto01 = data.frame(Auto, mpg01)
Auto01$color = "black"
Auto01$color[mpg01] = "red"

# Examining the pairwise plots, we see that many of the
# variables show correlation with mpg01, especially
# displacement, horsepower, and weight:
pairs(Auto01[-11], col=Auto01$color)
dev.copy(png,'./Output/Ex11_Pairs.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# Creating training and testing data sets:
train_ind = sample(seq_len(nrow(Auto01)), floor(0.75*nrow(Auto01)))
train = Auto01[train_ind,]
test = Auto01[-train_ind,]

# LDA:
fit1 = lda(mpg01~displacement+horsepower+weight+acceleration, data=train, family=binomial)
pred1 = predict(fit1, test)
table(pred1$class, test$mpg01)
mean(pred1$class == test$mpg01)

# QDA:
fit2 = qda(mpg01~displacement+horsepower+weight+acceleration, data=train, family=binomial)
pred2 = predict(fit2, test)
table(pred2$class, test$mpg01)
mean(pred2$class == test$mpg01)

# Logistic regression:
fit3 = glm(mpg01~displacement+horsepower+weight+acceleration, data=train, family=binomial)
resp3 = predict(fit3, test, type="response")
pred3 = resp3 > 0.5
table(pred3, test$mpg01)
mean(pred3 == test$mpg01)

# KNN k=1:
set.seed(1)
Xtrain = cbind(train$displacement, train$horsepower, train$weight, train$acceleration)
Xtest = cbind(test$displacement, test$horsepower, test$weight, test$acceleration)
Ytrain = train$mpg01
pred4 = knn(Xtrain, Xtest, Ytrain, k=1)
table(pred4, test$mpg01)
mean(pred4 == test$mpg01)

# Enlarging k:
kvals = seq(1:25)
classification = seq(1:25)
for(i in 1:25) {
  pred = knn(Xtrain, Xtest, Ytrain, k=i)
  classification[i] = mean(pred == test$mpg01)
}
plot(kvals, classification)
dev.copy(png,'./Output/Ex11_KNN_Classification_Rate_vs_K.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()
