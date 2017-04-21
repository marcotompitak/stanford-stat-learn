# Loading in the data
Auto = read.table("../Data/Auto.data", header=TRUE, na.strings="?")
rownames(Auto)=Auto[,9]
Auto=Auto[,-9]
attach(Auto)

# Exercise 8

# Linear fit of mpg vs horsepower
par(mfrow=c(1,1))
fit1 = lm(mpg~horsepower)
summary(fit1)
plot(horsepower,mpg)
abline(fit1, col="red")
dev.copy(png,'./Output/Ex8_mpg_vs_horsepower.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# Predicting mpg at horsepower=98, with prediction and confidence intervals
predict(fit1,data.frame(horsepower=98), interval="predict")
predict(fit1,data.frame(horsepower=98), interval="confidence")

# Diagnostic plots show us that there are non-linearities, as was already
# clear from the plot of mpg vs horsepower itself.
par(mfrow=c(2,2))
plot(fit1)
dev.copy(png,'./Output/Ex8_mpg_vs_horsepower_fit1diagnostics.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()


# Exercise 9

# Multiple linear regression of mpg vs all other parameters in the dataset
fit2 = lm(mpg~., Auto)
summary(fit2)

# We see strong predictors, but some do not seem significant. If we remove
# acceleration and cylinders, we are left with only coefficients with
# p-values < 0.05.
fit3 = update(fit2,mpg~.-acceleration-cylinders)
summary(fit3)
plot(fit3)
dev.copy(png,'./Output/Ex8_mpg_vs_horsepower_fit3diagnostics.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# Looking at the diagnostics plots, we see that there are some non-linearities,
# as well as points with extreme legerage.
# Considering interactions, especially interactions between displacement and
# horsepower seem to be significant.
fit4 = update(fit3, mpg~.+displacement*horsepower+horsepower*year+I(displacement^2)*I(horsepower^2))
summary(fit4)
plot(fit4)
dev.copy(png,'./Output/Ex8_mpg_vs_horsepower_fit4diagnostics.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# Second-order terms in displacement and horsepower individually are also
# significant, but only in the absence of the higher-order interaction term,
# as we saw above. They do not improve the non-linearities shown by the
# residuals as much as the interacting terms.
fit5 = update(fit3, mpg~.+I(displacement^2)+I(horsepower^2))
summary(fit5)
plot(fit5)
dev.copy(png,'./Output/Ex8_mpg_vs_horsepower_fit5diagnostics.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()

# Neither do square root terms and logarithmic terms.
fit6 = update(fit3, mpg~.+I(displacement^(1/2))+I(horsepower^(1/2)))
summary(fit6)
plot(fit6)
fit7 = update(fit3, mpg~.+I(log(displacement))+I(log(horsepower)))
summary(fit7)
plot(fit7)
