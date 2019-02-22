
poisson_pdf <- function(k, lambda){
  pdf = (exp(-lambda) * lambda^k)/factorial(k)
  return(pdf)
}

gamma_pdf <- function(lambda, a, b) {
  pdf = (b^a * lambda^(a-1) * exp(-b*lambda))/factorial(a-1)
  return(pdf)
}

k = 0:20
lambda = 0:10

# 1.2 Plot Poisson
plot_uno <- plot(k, poisson_pdf(k,4))
plot_dos <- plot(k, poisson_pdf(k,7))
plot_tres <- plot(k, poisson_pdf(k,10))

# 1.3 Plot Gamma
plot_four <- plot(lambda, gamma_pdf(lambda,1,1))
plot_five <- plot(lambda, gamma_pdf(lambda,1,2))
plot_six <- plot(lambda, gamma_pdf(lambda,9,0.5))

# Plot posterior, two ways
# 1.4
lambda = 0:20
alpha = 7.5
beta = 1
k = 7
posterior = c(0:20)
evidence = 0
for (l in lambda){
  evidence = evidence + (poisson_pdf(k, l) * gamma_pdf(l,alpha, beta))
}
for (l in lambda){
  posterior[l+1] = (poisson_pdf(k, l) * gamma_pdf(l,alpha, beta))/evidence
}

plot(lambda, posterior)

# 1.5
posterior = gamma_pdf(lambda, alpha + 7, beta + 1)
plot(lambda, posterior)








