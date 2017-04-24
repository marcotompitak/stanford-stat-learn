# Loading in the data
library(MASS)
attach(Boston)

# Creating a new categorical value for crim:
crim01 = crim > median(crim)
Boston01 = data.frame(Boston, crim01)
Boston01$color = "black"
Boston01$color[crim01] = "red"

# Examining the pairwise plots, the correlations with
# crim01 are not so clear cut:
pairs(Boston01[-16], col=Boston01$color)
dev.copy(png,'./Output/Ex13_Pairs.png', width = 25, height = 25, units = 'cm', res = 300); dev.off()

# We therefore look at the box plots, and we see strong
# correlation with indus, rm, age, dis, rad, tax, pratio,
# black, lstat and medv.
par(mfrow=c(4,4), mar=c(4, 4, 1, 1))
for(i in names(Boston)) {
  boxplot(crim01, Boston[,i], xlab="crim01", ylab=i)
}
dev.copy(png,'./Output/Ex13_Boxplots.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# Creating training and testing data sets:
train_ind = sample(seq_len(nrow(Boston01)), floor(0.75*nrow(Boston01)))
train = Boston01[train_ind,]
test = Boston01[-train_ind,]

# LDA:
fit1 = lda(crim01~indus+rm+age+dis+rad+tax+ptratio+black+lstat+medv, data=train, family=binomial)
pred1 = predict(fit1, test)
table(pred1$class, test$crim01)
mean(pred1$class == test$crim01)

# QDA:
fit2 = qda(crim01~indus+rm+age+dis+rad+tax+ptratio+black+lstat+medv, data=train, family=binomial)
pred2 = predict(fit2, test)
table(pred2$class, test$crim01)
mean(pred2$class == test$crim01)

# Logistic regression:
fit3 = glm(crim01~indus+rm+age+dis+rad+tax+ptratio+black+lstat+medv, data=train, family=binomial)
resp3 = predict(fit3, test, type="response")
pred3 = resp3 > 0.5
table(pred3, test$crim01)
mean(pred3 == test$crim01)

# KNN k=1:
set.seed(1)
Xtrain = cbind(train$indus, train$rm, train$age, train$dis, train$rad, train$tax, train$ptratio, train$black, train$lstat, train$medv)
Xtest = cbind(test$indus, test$rm, test$age, test$dis, test$rad, test$tax, test$ptratio, test$black, test$lstat, test$medv)
Ytrain = train$crim01
pred4 = knn(Xtrain, Xtest, Ytrain, k=1)
table(pred4, test$crim01)
mean(pred4 == test$crim01)

# Enlarging k now actually makes the classification worse:
kvals = seq(1:25)
classification = seq(1:25)
for(i in 1:25) {
  pred = knn(Xtrain, Xtest, Ytrain, k=i)
  classification[i] = mean(pred == test$crim01)
}
plot(kvals, classification)
par(mfrow=c(1,1))
dev.copy(png,'./Output/Ex13_KNN_Classification_Rate_vs_K.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()
