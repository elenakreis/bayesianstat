
library('MCMCpack')

# 1-D Gaussian mixture model

# Number of data points and clusters
N = 5000
K = 4

# ?
a = 100
b = 2


mu_hat = 0
sd_hat = 10

alpha = 1

# ?
pi = rdirichlet(1, rep(alpha, K))

# For each cluster, assign a mean and a width (variance)
mu = rep(0L, K)
sigma = rep(0L, K)
for (k in 1:K) {
  sigma[k] = rgamma(n=1, shape=a, rate=b)
  mu[k] = rnorm(1, mean = mu_hat, sd = sd_hat)
}

# Assign to each data point a cluster label and a value
z = rep(0, N)
x = rep(0, N)

for (i in 1:N) {
  z[i] = sample(1:K, size=1, replace = TRUE, prob = pi)
  x[i] = rnorm(1, mu[z[i]], sigma[z[i]])
}

# Plot the generated data
hist(x, breaks = 50)