# doesitwork.R

#setwd("C:/Users/u341138/Dropbox/Documents/Teaching/2017-2018/Bayesian statistics/R/DBDA2Eprograms")

# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

# Required packages for this exercise.
require(rjags)
require(coda)
source("DBDA2E-utilities.R")


plotGaussian <- function(data, mean, sd) {
  plot.new()
  par(mfrow=c(1,1))
  h = hist(data, breaks = 50)
  xrange = seq(min(data),max(data),length=50) 
  gaussian_distribution = dnorm(xrange, mean=mean, sd = sd)
  scaled_gaussian = gaussian_distribution * diff(h $ mids[1:2])*length(data) 
  lines(xrange, scaled_gaussian)
}

# This code provides an outline; additional code needs to be added by yourself!



#----------   Model 1: learning a mean   --------------

# Set the initial parameters

sigma = 2.0
N = 100
mu = 10

# Generate observations here
x = rep(0, N)

# fill vector x here


hist(x, breaks = 50)


# Write down the same model in JAGS notation.
model1.string = "
  model {
    # Prior
    

    # Likelihood
  }
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
model1.spec = textConnection(model1.string)

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel1 <- jags.model(model1.spec,
                         data = list('N' = N,                    # the number of data points
                                     'sigma' = sigma,            # the standard deviation of the likelihood
                                     'x' = x                     # the observations
                                     ),  
                         n.chains=4)

mcmciterations = 10000 
samples = coda.samples(jagsmodel1,
                       c(''), # which variables do you want to monitor?
                       n.iter=mcmciterations)

# plot/interpret the results



#----------   Model 2: learning mean and variance   --------------

# Set the initial parameters
sigma = 2.0
N = 1000
mu = 10

# Generate observations here
x = rep(0, N)



hist(x, breaks = 50)


# Write down the same model in JAGS notation.
model1.string = "
  model {
    # Prior
    

    # Likelihood
    
    
  }
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
model1.spec = textConnection(model1.string)

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel1 <- jags.model(model1.spec,
                         data = list('N' = N,                    # the number of data points
                                     'x' = x                     # the observations
                         ),  
                         n.chains=4)

mcmciterations = 10000 
samples = coda.samples(jagsmodel1,
                       c(''), # which parameters do you want to monitor?
                       n.iter=mcmciterations)



# plot/interpret the results



#----------   Model 3: the wrong model   --------------


# Set the initial parameters
N = 1000
df = 1

# Generate observations here
x = rep(0, N)


hist(x, breaks = 50)


# Write down the same model in JAGS notation.
model3.string = "
model {
  # Prior
  
  
  # Likelihood
}
"

# JAGS usually reads models from a text file; here we use a string as a fake file.   
model3.spec = textConnection(model3.string)

# Construct the object containing both the model specification as well as the data and some sampling parameters.
jagsmodel3 <- jags.model(model3.spec,
                         data = list('N' = N,                    # the number of data points
                                     'x' = x                     # the observations
                         ),  
                         n.chains=4)

mcmciterations = 10000 
samples = coda.samples(jagsmodel3,
                       c(''), # which variables do you want to monitor?
                       n.iter=mcmciterations)


# plot/interpret the results
