# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

# --> set to your wd: setwd("")

# Required packages for this exercise.
require(rjags)
require(coda)

### Helper functions

# Plotting of standard deviation interval
plot_sd <- function(meanvec, sdvec) {
  n = length(meanvec)
  # Compute the upper end of the interval, i.e. expectation+sigma
  upper = meanvec + sdvec
  # Compute the lower end of the interval, i.e. expectation-sigma
  lower = meanvec - sdvec
  # Plot the interval
  polygon(c(1:n, rev(1:n)), c(lower, rev(upper)), col=rgb(0.2, 0.2, 0.2, 0.1), border=NA)
  # Plot the upper and lower bounds of the interval as a dotted line
  lines(1:n, lower, lty=3, col=rgb(0.1, 0.1, 0.1))
  lines(1:n, upper, lty=3, col=rgb(0.1, 0.1, 0.1))
}



# Example usage of plot_sd()
n = 100
sigma = 1
x = rnorm(n, 0, sigma)
mean_vector = rep(0,n)
plot(x, type='l', xaxs='i')
plot_sd(mean_vector, rep(sigma, n))



load(file='timeseries.Rdata')
n = length(y)


# Time series exercise 1

plot(y, pch=19, xlab='Observation index', type='o')


model_notimeseries = "
model {
  tau ~ dgamma(0.1, 0.1)
  sigma = 1/sqrt(tau)
  mu ~ dnorm(0, 1)
  for(i in 1:n){
    y[i] ~ dnorm(mu, tau)
  }
}
"

model_timeseries = "
model {
  tau ~ dgamma(0.1, 0.1)
  sigma = 1/sqrt(tau)
  mu ~ dnorm(0, 1)
  y[1] ~ dnorm(mu, tau)
  for(i in 2:n){
    y[i] ~ dnorm(y[i-1], tau)
  }
}
"

niter=10000
nchains=4
nsamples = niter*nchains

data <- list('n' = n, 'y' = y) # to be passed on to JAGS
parameters <- c('mu','sigma') # fill in
jagsmodel <- jags.model(textConnection(model_notimeseries), 
                        data = data, 
                        n.chains = nchains)
samples = as.matrix(coda.samples(jagsmodel, parameters, n.iter = niter))

# Compute posterior expectation of the vector mu:
mu_expect = mean(samples[,'mu'])

# Compute posterior expectation of the standard deviation sigma:
sigma_expect = mean(samples[,'sigma'])

# Plot the actual data y, the posterior expectation, and the certainty interval (using plot_sd, see top of script)
plot(y, pch=19, xlab='Observation index', type='o')
plot_sd(rep(mu_expect,n), rep(sigma_expect,n))
abline(h=mu_expect, col = 'red')

# Repeat for the other model
jagsmodel <- jags.model(textConnection(model_timeseries), 
                        data = data, 
                        n.chains = nchains)
samples = as.matrix(coda.samples(jagsmodel, parameters, n.iter = niter))

mu_expect = mean(samples[,'mu'])
sigma_expect = mean(samples[,'sigma'])

# The expectation is related to the value before, and for the first item it is just the mean. Therefore shift y to the right by 1 and add mu to the beginning
mean_vect = y[1:n-1]
mean_vect = append(mean_vect, mu_expect, after=0)

plot(y, pch=19, xlab='Observation index', type='o')
plot_sd(mean_vect, rep(sigma_expect,n))
lines(seq(1,n), mean_vect, col = 'red')

# 3.1.3
# Model comparison:
model_timeseries_or_notimeseres = "
model {
  tau ~ dgamma(0.1, 0.1)
  sigma = 1/sqrt(tau)
  mu0 ~ dnorm(0, 1)

  m ~ dbern(0.5)

  mu[1] = mu0
  y[1] ~ dnorm(mu0, tau)

  for(i in 2:n){
    mu[i] = ifelse(m==0, mu0, y[i-1])
    y[i] ~  dnorm(mu[i],tau)
  }
}

"

niter=10000
nchains=4
nsamples = niter*nchains

data <- list('n' = n, 'y' = y) # to be passed on to JAGS
parameters <- c('m') # fill in
jagsmodel <- jags.model(textConnection(model_timeseries_or_notimeseres),
                        data = data, 
                        n.chains = nchains)
samples = as.matrix(coda.samples(jagsmodel, parameters, n.iter = niter))


# Compute the Bayes factor of the comparison:

post_m0 = sum(samples) / length(samples)
post_m1 = 1 - post_m0
bayes_factor = post_m1 / post_m0

