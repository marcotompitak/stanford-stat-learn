# Loading the data
library(ISLR)
attach(Default)

# Standard errors using glm:
coef(summary(glm(default~income+balance, family="binomial")))

# Standard errors using bootstrap are very similar:
library(boot)
parfunc = function(data, index) {
  b1 = with(data[index,], coef(summary(glm(default~income+balance, family="binomial")))["income","Estimate"])
  b2 = with(data[index,], coef(summary(glm(default~income+balance, family="binomial")))["balance","Estimate"])
  return(c(b1,b2))
}
set.seed(1)
boot(Default, parfunc, 100)