# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

#setwd("C:/Users/Mihaela/Desktop/Uni/B2/Sem 2/Bayesian Statistics/DBDA2Eprograms/DBDA2Eprograms")

# Required packages for this exercise.
require(rjags)
require(coda)


# Load data
load(file = "twolines.Rdata")
n = length(y)

## -------- Linear regression --------


linreg_model1 = "model{
  # Prior
  w0 ~ dnorm(0, 1)
  w1 ~ dnorm(0, 1)
  tau ~ dgamma(0.01, 0.01)
  
  
  # Likelihood
  for(i in 1:n){
    mu[i] <- w0 + w1*x[i]
    y[i]   ~ dnorm(mu[i], tau)
  }
}
"

niter = 10000
nchains = 4

# Create your data structure here
data = list('n' = n, 'x' = x, 'y'= y)

jagsmodel_linreg1 <- jags.model(textConnection(linreg_model1), 
                                data = data,
                                n.chains = nchains)

store_parameters = c('w0', 'w1')

# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_oneline = coda.samples(jagsmodel_linreg1, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_oneline)


summary(samples_oneline)

plot(x,y,pch=20)

# Generate random indices for 500 samples
rand_indices = sample(1:dim(samplesMatrix)[1], 500, replace = F)
for (i in 1:500){
  w0 = samplesMatrix[rand_indices[i],1]
  w1 = samplesMatrix[rand_indices[i],2]
  abline(w0, w1,  col=rgb(0.1, 0.5, 0.1, max = 1.0, alpha = 0.05))
}
abline(mean(samplesMatrix[,1]), mean(samplesMatrix[,2]), col = 'red')


#The least uncertainty is in the middle, as all the lines all close together. This is also the part where 
# the model fits the data best.





## -------- Linear regression mixture --------

linreg_model2 = "model{
  # Prior
  for(i in 1:n){
    z[i] ~ dcat(c(0.5, 0.5))
  }
  for(i in 1:2){
    w0[i] ~ dnorm(0, 1)
    w1[i] ~ dnorm(0, 1)
  }
  tau ~ dgamma(0.01, 0.01)
  
  
  # Likelihood
  for(i in 1:n){
    mu[i] <- w0[z[i]] + w1[z[i]]*x[i]
    y[i]   ~ dnorm(mu[i], tau)
  }
}
"


niter = 10000
nchains = 4
# Create your data structure here
data = list('n' = n, 'x' = x, 'y'= y)

jagsmodel_linreg2 <- jags.model(textConnection(linreg_model2), 
                                data = data,
                                n.chains = nchains)

store_parameters = c('w0', 'w1')



# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_twolines = coda.samples(jagsmodel_linreg2, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_twolines)

summary(samples_twolines)

plot(x,y,pch=20)

# Generate random indices for 500 samples
rand_indices = sample(1:dim(samplesMatrix)[1], 500, replace = F)
for (i in 1:500){
  w0_1 = samplesMatrix[rand_indices[i],1]
  w1_1 = samplesMatrix[rand_indices[i],3]
  
  w0_2 = samplesMatrix[rand_indices[i],2]
  w1_2 = samplesMatrix[rand_indices[i],4]
  abline(w0_1, w1_1,  col=rgb(0.1, 0.5, 0.1, max = 1.0, alpha = 0.05))
  abline(w0_2, w1_2,  col=rgb(0.5, 0.1, 0.1, max = 1.0, alpha = 0.05))
}
abline(mean(samplesMatrix[,1]), mean(samplesMatrix[,3]), col = 'yellow')
abline(mean(samplesMatrix[,2]), mean(samplesMatrix[,4]), col = 'blue')

## -------- Model selection --------

linreg_model3 = "model{
  #Prior
  w0_m1 ~ dnorm(0, 1)
  w1_m1 ~ dnorm(0, 1)
  
  for(i in 1:n){
    z[i] ~ dcat(c(0.5, 0.5))
  }
  for(i in 1:2){
    w0_m2[i] ~ dnorm(0, 1)
    w1_m2[i] ~ dnorm(0, 1)
  }
  
  tau ~ dgamma(0.01, 0.01)
  
  m ~ dcat(c(0.5, 0.5))
  
  #Likelihood
  
  for(i in 1:n){
    mu[i] <- equals(m,1) * (w0_m1 + w1_m1*x[i]) + equals(m,2) * (w0_m2[z[i]] + w1_m2[z[i]]*x[i])
    y[i]   ~ dnorm(mu[i], tau)
  }
}
"

niter = 10000
nchains = 4
# Create your data structure here
data = list('n' = n, 'x' = x, 'y' = y)

jagsmodel_linreg3 <- jags.model(textConnection(linreg_model3), 
                                data = data,
                                n.chains = nchains)

store_parameters = c('m', 'w0_m1', 'w1_m1', 'w0_m2', 'w1_m2')

# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_oneortwolines = coda.samples(jagsmodel_linreg3, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_oneortwolines)

rand_indices = sample(1:dim(samplesMatrix)[1], 250, replace = F)

plot(x,y,pch=20)
for(i in 1:250){
  w0_m1 = samplesMatrix[rand_indices[i],2]
  w1_m1 = samplesMatrix[rand_indices[i],5]
  
  w0_m2_l1 = samplesMatrix[rand_indices[i],3]
  w1_m2_l1 = samplesMatrix[rand_indices[i],6]
  
  w0_m2_l2 = samplesMatrix[rand_indices[i],4]
  w1_m2_l2 = samplesMatrix[rand_indices[i],7]
  
  abline(w0_m1, w1_m1, col=rgb(0.1, 0.1, 0.5, max = 1.0, alpha = 0.05))
  abline(w0_m2_l1, w1_m2_l1,  col=rgb(0.1, 0.5, 0.1, max = 1.0, alpha = 0.05))
  abline(w0_m2_l2, w1_m2_l2,  col=rgb(0.5, 0.1, 0.1, max = 1.0, alpha = 0.05))
}


posterior_model1_mcmc = sum(samplesMatrix[,1]==1) / dim(samplesMatrix)[1]
posterior_model2_mcmc = 1 - posterior_model1_mcmc
bayes_factor = posterior_model2_mcmc/posterior_model1_mcmc