# Loading the data
library(MASS)

# Plotting pairs
pairs(Boston)
dev.copy(png,'./Output/Ex10_Pairs.png', width = 30, height = 30, units = 'cm', res = 300); dev.off()

# Correlation of all variables with crime rate, sorted
cor(Boston)[,1][order(-abs(cor(Boston)[,1]))]

# We see a range of correlation strengths, primarily with rad, tax, lstat and nox
par(mfrow=c(2,2))
plot(Boston$rad,Boston$crim, ylab = "Crime", xlab = "Rad index")
plot(Boston$tax,Boston$crim, ylab = "Crime", xlab = "Tax rate")
plot(Boston$lstat,Boston$crim, ylab = "Crime", xlab = "Percentage lower status")
plot(Boston$nox,Boston$crim, ylab = "Crime", xlab = "Nitrogen Oxide (ppm)")
dev.copy(png,'./Output/Ex10_MainCrimeCorrelators.png', width = 15, height = 15, units = 'cm', res = 300); dev.off()
# Crime rate seems to be highest in areas where people are not well off: remote areas
# where taxes are relatively high, many people are of 'lower status', and there is
# much pollution. There is also strong correlation with the amount of industry, so
# this pollution is likely due to the proximity to heavy industry.

# To see if any suburbs stand out in any particular variable, we can create box plots
# and see which parameters have strong outliers. We see that several variables have
# distributions with a very long tail, or significant outliers, such as crime rate,
# percentage of residential land zoning, distance to employment centers and number
# of black people.
par(mfrow=c(4,4), mar=c(1, 4, 1, 1) + 0.1)
for(i in names(Boston)){
  boxplot(Boston[i], main=i)
}
dev.copy(png,'./Output/Ex10_Boxplots.png', width = 25, height = 25, units = 'cm', res = 300); dev.off()

# Number of suburbs bounding the Charles river (chas==1):
sum(Boston$chas==1)

# Median pupil-teacher ratio:
median(Boston$ptratio)

# Looking at the lowest medv, we see that two suburbs share the
# lowest value of $5000.
Boston$medv[order(Boston$medv)]

# We want to look at the other parameters for this low-medv suburb.
# Arbitrarily taking one of the two having medv=5.0, we can set up
# the empirical cumulative distribution function for each parameter
# and see where the value corresponding to this suburb lies. We see
# that for most variables, the value of this particular suburb is
# in a low or in a high percentile, depending on the direction of
# the correlation that likely exists between medv and the variable
# in question.
lowest_medv_suburb = Boston[Boston$medv==5.0,][1,]
for(i in names(Boston)){
  cat(sprintf("%s, %f\n", i, ecdf(Boston[,i])(lowest_medv_suburb[,i])))
}

# We now consider suburbs where houses average to 8 rooms or more.
# There are several such suburbs and we will take the average values
# of all the other parameters over them to get an idea of a typical
# such suburb. Looking again at which percentile we find these typical
# values in, we see a few strong effects.
large_homes = Boston[Boston$rm>=8,]
large_homes_avg = colMeans(large_homes)
for(i in names(Boston)){
  cat(sprintf("%s, %f\n", i, ecdf(Boston[,i])(large_homes_avg[i])))
}
# First, these suburbs tend to be on the Charles river. Second, they
# tend to have a low pupil-to-teacher ratio. Third, they contain few
# residents of 'lower status'. And finally and most obviously, the
# houses are very valuable.