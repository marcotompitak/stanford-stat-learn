# Loading in the data:
library(MASS)
attach(Boston)

# Getting the p-value of the t-statistic for linear fits
# of crim vs all other parameters. We see that all show
# significant correlation except chas.
par(mfrow=c(4,4), mar=c(4, 4, 1, 1))
estimates_single = structure(1:13, names=names(Boston[,-1]))
for(x in names(Boston[,-1])) {
  y = Boston[,x]
  fit = lm(crim~y)
  estimates_single[x] = coef(summary(fit))["y", "Estimate"]
  print(x)
  print(coef(summary(fit))["y","Pr(>|t|)"])
  plot(Boston[,x],crim, xlab = x, ylab="crime")
  abline(fit, col="red")
}
dev.copy(png,'./Output/Ex15_Crim_vs_All_fits.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# Now we perform multiple regression with all other parameters.
# We see that only a few predictors show significant correlation:
# zn, dis, rad, black and medv.
multi_fit = lm(crim~., data=Boston)
summary(multi_fit)

# The slope estimates for the different predictors vary significantly
# for some of the predictors, especially nox:
estimates_multiple = coef(summary(multi_fit))[names(Boston[,-1]),"Estimate"]
par(mfrow=c(1,1))
plot(estimates_single, estimates_multiple, xlab = "Single Regression Slopes", ylab = "Multiple Regression Slopes")
abline(0,1, col="gray") # Ideally the points lie on this line
dev.copy(png,'./Output/Ex15_Single_vs_Multiple_estimattes.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()


# We now generate third-order polynomial fits, to assess non-linearities.
for(x in names(Boston[,-1][,-3])) {
  print(x)
  y = Boston[,x]
  
  # Third order fit:
  fit = lm(crim~poly(y,3))
  
  # For each order, check if we can reject the null hypothesis
  # at confidence level 0.01:
  ords = c()
  if(coef(summary(fit))["poly(y, 3)1","Pr(>|t|)"] < 0.01) {
    ords = c(ords, " linear")
  }
  if(coef(summary(fit))["poly(y, 3)2","Pr(>|t|)"] < 0.01) {
    ords = c(ords, " quadratic")
  }
  if(coef(summary(fit))["poly(y, 3)3","Pr(>|t|)"] < 0.01) {
    ords = c(ords, " cubic")
  }
  
  # Print which orders are significant:
  cat("Significant orders:")
  cat(paste(ords, collapse=","))
  cat("\n")
}
