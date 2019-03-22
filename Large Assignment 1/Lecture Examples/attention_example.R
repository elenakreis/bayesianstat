# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

# Required packages for this exercise.
require(rjags)
require(coda)



mu1 = 40
mu2 = 30
sd = 10
T = 256
t_change = 192

c <- rep(0, T)

for (t in 1:T) {
  c[t] <- rnorm(n = 1, mean = (t<t_change)*mu1 + (t>= t_change)*mu2, sd=sd)
}

par()
plot(1:T, c, type='l', xlab='Time t', ylab = 'Response c', main = expression(paste('Data and ', tau)), xlim=c(1, T))
abline(v=t_change, 'col' = rgb(0.0, 0.0, 0.0), 'lwd' = 3)



ms <- "
model {
  
  prec ~ dgamma(0.001, 0.001)
  mu[1] ~ dnorm(0, 0.001)
  mu[2] ~ dnorm(0, 0.001)

  t_change ~ dunif(1, T)
  for (t in 1:T) {
    z[t] <- ifelse(t < t_change, 1, 2)
    c[t] ~ dnorm(mu[z[t]], prec)
  }

  sigma <- 1/sqrt(prec)
}
"




jags_dat <- list('c' = c, 'T' = T)
model <- jags.model(textConnection(ms), jags_dat)

nsamples <- 5000
params <- c('t_change', 'sigma', 'mu')
samples <- do.call('rbind', coda.samples(model, variable.names = params, n.iter = nsamples))

nsamples2show <- 100

par(mfrow=c(1,3), pty='s')
hist(samples[,'t_change'], breaks = 20, xlab = 't_change', ylab = 'Density', probability = T, 
           main = expression(paste('Distribution of ', tau)), col = rgb(1.0, 0.0, 0.0), xlim = c(0, 250))
abline(v = t_change, col=rgb(0.0, 0.0, 0.0), lwd=2)
plot(1:T, c, type='l', xlab='Time t', ylab = 'Response c', main = expression(paste('Data and samples of ', tau)))
for (i in 1:nsamples2show) {
  ix <- sample(1:(nsamples), size = 1, replace = T)
  abline(v=samples[ix, 't_change'], col = rgb(1.0, 0.0, 0.0, alpha=0.1))  
}
abline(v=t_change, col = rgb(0.0, 0.0, 0.0), lwd = 2)

plot(1:T, c, type='l', xlab='Time t', ylab = 'Response c', main = expression(paste('Data and samples of ', tau, ' (zoom)')),
     xlim = c(180, 210))
for (i in 1:nsamples2show) {
  ix <- sample(1:(nsamples), size = 1, replace = T)
  abline(v=samples[ix, 't_change'], col = rgb(1.0, 0.0, 0.0, alpha=0.1))  
}
abline(v=t_change, col = rgb(0.0, 0.0, 0.0), lwd = 2)
