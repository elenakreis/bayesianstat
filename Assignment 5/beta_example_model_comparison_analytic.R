# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

# Required packages for this exercise.
require(rjags)
require(coda)
#source("DBDA2E-utilities.R")


## The distributions that we use here are available in R, but for clarity are made explicit here.

# Bernoulli(z,N | theta)
bernoulli_pdf <- function(theta, z, N) {
  # Computes p(z,N|theta) = Bernoulli(theta|z,N)
  pdf = theta^z * (1 - theta)^(N - z)
  return(pdf)
}

# beta(theta | a, b)
beta_pdf <- function(theta, a, b) {
  # Computes p(theta|a,b) = beta(theta|a,b)
  pdf = theta^(a - 1) * (1 - theta)^(b - 1) / beta(a,b)
  return(pdf)
}

# B(a+z, N-z+b) / B(a,b)
beta_bernoulli_evidence <- function(a, b, N, z) {
  return( beta( z+a, N-z+b ) / beta( a, b ) ) # not numerically stable
}


# Define a range of theta's to compute the probability density for:
theta = seq(0, 1, by = 0.01)

# Observations: 6 heads out of 9 flips
z = 6
N = 9

# Hyperparameters

# Model 1
a1 = 3
b1 = 8

# Model 2
a2 = 9
b2 = 2


likelihood                  = bernoulli_pdf(theta, z = z, N = N)
prior1                      = beta_pdf(theta, a = a1, b = b1)
prior2                      = beta_pdf(theta, a = a2, b = b2)
posterior1                  = beta_pdf(theta, a = z + a1, b = N - z + b1) # Easy posterior, because conjugacy
posterior2                  = beta_pdf(theta, a = z + a2, b = N - z + b2) # Easy posterior, because conjugacy

# Plot the probability density functions

par(mfrow=c(2,3))
plot(theta, prior1, type='l')
title('Prior 1 (a=3, b=8)')
plot(theta, likelihood, type='l')
title('Likelihood (z=6, N=9)')
plot(theta, posterior1, type='l')
title('Posterior 1 (a=3+z, b=8+N-z)')
plot(theta, prior2, type='l')
title('Prior 2 (a=9, b=2)')
plot(theta, likelihood, type='l')
title('Likelihood (z=6, N=9)')
plot(theta, posterior2, type='l')
title('Posterior 2 (a=9+z, b=2+N-z)')

## NOW WE DO MODEL COMPARISON

evidence1 = beta_bernoulli_evidence(a1, b1, N, z)
show(sprintf('Evidence p(D|m_1) = %0.5f', evidence1))
evidence2 = beta_bernoulli_evidence(a2, b2, N, z)
show(sprintf('Evidence p(D|m_2) = %0.5f', evidence2))

bayes_factor = evidence1 / evidence2
show(sprintf('Bayes factor = %0.5f', bayes_factor))
show(sprintf('>1 : in favor of m1, <1 : in favor of m2'))

# I have no model preference
prior_model_1 = 0.5
prior_model_2 = 0.5

prior_odds = prior_model_1 / prior_model_2 # trivially 1.0 here




posterior_odds = bayes_factor * prior_odds # multiply with 1.0 is not very exciting

posterior_model1 = posterior_odds / (1.0 + posterior_odds)
posterior_model2 = 1 - posterior_model1

show(sprintf('Posterior model 1 p(m_1 | D) = %0.2f', posterior_model1))
show(sprintf('Posterior model 2 p(m_2 | D) = %0.2f', posterior_model2))

