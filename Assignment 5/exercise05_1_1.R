logistic <- function(x, k, x0){
  func = 1/(1+ exp(-k*x + k*x0))
  return(func)
}


k= 1.3
x0 = -8
x = -10:10

plot_uno <- plot(x, logistic(x,k,x0), type='l', main = paste('k = ', k, ', x_0 = ', x0))