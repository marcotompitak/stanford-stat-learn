# Load the data
Auto = read.table("../Data/Auto.data", header=TRUE, na.strings="?")

# Function to return range, mean and standard deviation
RangeMeanSD <- function(x){
  r = range(x, na.rm=TRUE)
  m = mean(x, na.rm=TRUE)
  s = sd(x, na.rm=TRUE)
  return(c(r,m,s))
}

# Ranges, means and std devs of the quantitative variables
RangeMeanSD(Auto$mpg)
RangeMeanSD(Auto$cylinders)
RangeMeanSD(Auto$displacement)
RangeMeanSD(Auto$horsepower)
RangeMeanSD(Auto$weight)
RangeMeanSD(Auto$acceleration)
RangeMeanSD(Auto$year)

# Dropping rows 10 through 85
Auto_Reduced = Auto[-seq(10,85,1),]

RangeMeanSD(Auto_Reduced$mpg)
RangeMeanSD(Auto_Reduced$cylinders)
RangeMeanSD(Auto_Reduced$displacement)
RangeMeanSD(Auto_Reduced$horsepower)
RangeMeanSD(Auto_Reduced$weight)
RangeMeanSD(Auto_Reduced$acceleration)
RangeMeanSD(Auto_Reduced$year)

# Investigating correlations
pairs(Auto[,1:8])
dev.copy(png,'./Output/Ex9_Pairs.png', width = 20, height = 20, units = 'cm', res = 300); dev.off()

# There are strong correlations between all the technical parameters, e.g.
par(mfrow=c(2,2))
boxplot(Auto$horsepower~Auto$cylinders, xlab="Cylinders (no.)", ylab="Horsepower")
plot(Auto$acceleration~Auto$horsepower, xlab="Horsepower", ylab="Acceleration (s)")
boxplot(Auto$displacement~Auto$cylinders, xlab="Cylinders (no.)", ylab="Displacement (in^3)")
plot(Auto$horsepower~Auto$weight, xlab="Weight (lbs)", ylab="Horsepower")
dev.copy(png,'./Output/Ex9_TechnicalCorrs.png', width = 10, height = 10, units = 'cm', res = 300); dev.off()

# Some parameters also correlate with time, and with where the car was produced
par(mfrow=c(2,2))
boxplot(Auto$mpg~Auto$year, xlab="Year", ylab="Miles per gallon")
boxplot(Auto$acceleration~Auto$year, xlab="Year", ylab="Acceleration (s)")
# Added xaxt="n" to suppress labels on x-tics; then replaced the numerical labels with
# more informative names
boxplot(Auto$mpg~Auto$origin, xlab="Origin", ylab="Miles per gallon", xaxt="n")
axis(1, at=1:3, labels=c("U.S.", "Europe", "Japan"))
boxplot(Auto$horsepower~Auto$origin, xlab="Origin", ylab="Horsepower", xaxt="n")
axis(1, at=1:3, labels=c("U.S.", "Europe", "Japan"))
dev.copy(png,'./Output/Ex9_SoftCorrs.png', width = 10, height = 10, units = 'cm', res = 300); dev.off()

# Gas mileage correlates with almost all the other variables
cor(Auto$mpg,Auto$cylinders, use="complete.obs")
cor(Auto$mpg,Auto$displacement, use="complete.obs")
cor(Auto$mpg,Auto$horsepower, use="complete.obs")
cor(Auto$mpg,Auto$weight, use="complete.obs")
cor(Auto$mpg,Auto$acceleration, use="complete.obs")
cor(Auto$mpg,Auto$year, use="complete.obs")
# Origin is a categorical value, but happens to be ordered in just the right way
# to correlate with gas mileage
cor(Auto$mpg,Auto$origin, use="complete.obs")
# Many of the technical parameters are intercorrelated, however, so a prediction
# model may not need to take all of them into account.
