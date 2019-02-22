# 2.1
prior = function(theta) {
  p = (2 * (cos(4*pi*theta)+1)^2) / 3
  return (p)
}
theta = seq(0,1,0.01)
plot(theta, prior(theta))