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
}

"

model_timeseries = "
model {
}

"

niter=10000
nchains=4
nsamples = niter*nchains

data <- list('n' = n, 'y' = y) # to be passed on to JAGS
parameters <- c() # fill in
jagsmodel <- jags.model(textConnection(model_timeseries), # change to model_notimeseries if you want to plot the results for the simple model
                        data = data, 
                        n.chains = nchains)
samples = as.matrix(coda.samples(jagsmodel, parameters, n.iter = niter))

# Compute posterior expectation of the vector mu:

# Compute posterior expectation of the standard deviation sigma:

# Plot the actual data y, the posterior expectation, and the certainty interval (using plot_sd, see top of script)


# Repeat for the other model


# Model comparison:
model_timeseries_or_notimeseres = "
model {
}

"

niter=10000
nchains=4
nsamples = niter*nchains

data <- list('n' = n, 'y' = y) # to be passed on to JAGS
parameters <- c() # fill in
jagsmodel <- jags.model(textConnection(model_timeseries_or_notimeseres), # change to model_notimeseries if you want to plot the results for the simple model
                        data = data, 
                        n.chains = nchains)
samples = as.matrix(coda.samples(jagsmodel, parameters, n.iter = niter))


# Compute the Bayes factor of the comparison:

