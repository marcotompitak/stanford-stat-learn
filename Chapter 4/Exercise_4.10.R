# Loading in the data
library(ISLR)
library(MASS)
library(class)
attach(Weekly)

# We don't see any clear correlation between
# the direction and the other variables:
pairs(Weekly, col=Direction)
dev.copy(png,'./Output/Ex10_Pairs.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# Performing logistic regression, it seems only one
# predictor is statistically significant, namely
# (and surprisingly) Lag2:
fit1 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial)
summary(fit1)

# We see that our classifier has a classification rate of
# about 56%, and does especially poorly on classifying "Down"
# responses. The predictions are for a large part "Up".
resp = predict(fit1, Weekly, type="response")
pred = as.vector(factor(resp < 0.5, labels=c("Up", "Down")))
table(pred, Direction)
mean(pred == Direction)

# We now want to separate our data into training and testing sets:
train = Year<2009
fit2 = glm(Direction~Lag2, family=binomial, subset=train)
resp2 = predict(fit2, Weekly[!train,], type="response")
pred2 = as.vector(factor(resp2 < 0.5, labels=c("Up", "Down")))
table(pred2, Direction[!train])
mean(pred2 == Direction[!train])

# The same analysis using LDA. The performence is identical:
fit3 = lda(Direction~Lag2, subset=train)
pred3 = predict(fit3, Weekly[!train,])
table(pred3$class, Direction[!train])
mean(pred3$class == Direction[!train])

# Using QDA, our results are not very good:
fit4 = qda(Direction~Lag2, subset=train)
pred4 = predict(fit4, Weekly[!train,])
table(pred4$class, Direction[!train])
mean(pred4$class == Direction[!train])

# KNN does no better than random guessing with k=1:
set.seed(1)
pred5 = knn(matrix(Lag2[train], ncol=1), matrix(Lag2[!train], ncol=1), Direction[train], k=1)
table(pred5, Direction[!train])
mean(pred5 == Direction[!train])

# We can improve it by taking into account more neighbours:
kvals = seq(1:25)
classification = seq(1:25)
for(i in 1:25) {
  classification[i] = mean(knn(matrix(Lag2[train], ncol=1), matrix(Lag2[!train], ncol=1), Direction[train], k=i) == Direction[!train])
}
plot(kvals, classification)
abline(0.5, 0, col="red")
dev.copy(png,'./Output/Ex10_KNN_Classification_Rate_vs_K.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

