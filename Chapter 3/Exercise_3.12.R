# In general the regression slope for y~x and x~y will not
# be identical. Since we will approximately have a = 1/b
# if y = a*x and x = b*y, we must first of all have a = b = 1,
# otherwise the coefficients (assuming they are well estimated)
# cannot be the same:
y=2*x
coef(summary(lm(y~x)))
coef(summary(lm(x~y)))

# The slopes will also generally not be the same in the presence
# of an error term. The estimate for the slope is only symmetric
# under exchange of x and y if the sum of the squared residuals
# of x and y are identical (see Eq. 3.38). Therefore, even if
# x and y follow the line y=x on average, errors will make the
# slopes differ:
x=rnorm(100)
y=x+rnorm(100)
coef(summary(lm(y~x)))
coef(summary(lm(x~y)))

# The only way to get identical slopes (apart from random chance)
# is if y=x without any error:
x=rnorm(100)
y=x
coef(summary(lm(y~x)))
coef(summary(lm(x~y)))
