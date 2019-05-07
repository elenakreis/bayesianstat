# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

setwd("C:/Users/Mihaela/Desktop/Uni/B2/Sem 2/Bayesian Statistics/DBDA2Eprograms/DBDA2Eprograms")

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

w0_samples = samples_oneline[[1]][1:500,'w0']
w1_samples = samples_oneline[[1]][1:500,'w1']

summary(samples_oneline)

plot(x,y,pch=20)
for(i in 1:500){
  abline(w0_samples[i], w1_samples[i], col = rgb(0.1, 0.5, 0.1, max = 1.0, alpha = 0.05))
}
abline(1.2910, 0.7678, col = 'red')


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
}

for(i in 1:2){
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

w01_samples = samples_twolines[[1]][1:500,'w0[1]']
w11_samples = samples_twolines[[1]][1:500,'w1[1]']

w02_samples = samples_twolines[[1]][1:500,'w0[2]']
w12_samples = samples_twolines[[1]][1:500,'w1[2]']

summary(samples_twolines)

plot(x,y,pch=20)
for(i in 1:500){
  abline(w01_samples[i], w11_samples[i], col = rgb(0.1, 0.5, 0.1, max = 1.0, alpha = 0.05))
  abline(w02_samples[i], w12_samples[i], col = rgb(0.5, 0.1, 0.1, max = 1.0, alpha = 0.05))
}

abline(1.0897, 0.3471, col = 'yellow')
abline(0.3808, 1.0805, col = 'blue' )



## -------- Model selection --------

linreg_model3 = "model{

#Prior

w_0 ~ dnorm(0, 1)
w_1 ~ dnorm(0, 1)

for(i in 1:n){
z[i] ~ dcat(c(0.5, 0.5))
}


for(i in 1:2){
w0[i] ~ dnorm(0, 1)
w1[i] ~ dnorm(0, 1)
}

tau ~ dgamma(0.01, 0.01)


m ~ dcat(mPriorProb[])
mPriorProb[1] <- 0.5
mPriorProb[2] <- 0.5

#Likelihood

for(i in 1:n){
mu[i] <- equals(m,1) * (w_0 + w_1*x[i]) + equals(m,2) * (w0[z[i]] + w1[z[i]]*x[i])
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

store_parameters = c('m')

# Collect samples and store them in a matrix of niter*nchains by number-of-stored-parameters
samples_oneortwolines = coda.samples(jagsmodel_linreg3, store_parameters, n.iter = niter)
samplesMatrix = as.matrix(samples_oneortwolines)

plot(x,y,pch=20)
m_samples = samples_oneortwolines[[1]][1:250,'m']



aposterior_model1_mcmc = sum(m==1) / length(m)
posterior_model2_mcmc = 1 - posterior_model1_mcmc


nsamples = 250
plot(x,y,pch=20)


