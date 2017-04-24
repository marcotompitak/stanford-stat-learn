# 2^3:
Power = function() {
  print(2^3)
}
Power()

# x^a:
Power2 = function(x, a) {
  print(x^a)
}
Power2(3,8)
Power2(10,3)
Power2(8,17)
Power2(131,3)

# Returning the value so we can plot:
Power3 = function(x, a) {
  return(x^a)
}
plot(1:10, Power3(1:10,2), type="n", xlab="x", ylab = "x^2")
lines(1:10, Power3(1:10,2))

# A plotting function:
PlotPower = function(x, a) {
  plot(x, Power3(x,a), type="n", xlab="x", ylab = sprintf("x^%.1f",a))
  lines(x, Power3(x,a))
}
