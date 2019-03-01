library(Rlab)

N = 100
K = 4

alpha = rep(1/K, times=K)
a = c(1, 5, 1, 1)
b = c(1, 0, 1, 1)

theta = rep(0, times=K)
z = rep(0, times=N)
x = rep(0, times=N)

for(i in 1:K){
  theta[i] = rbeta(1, shape1 = a[i], shape2 = b[i])
}
for(i in 1:N){
  z[i] = sample(x = seq(1,K), size = 1, prob = alpha)
  x[i] = rbern(n = 1, prob = theta[z[i]])
}
