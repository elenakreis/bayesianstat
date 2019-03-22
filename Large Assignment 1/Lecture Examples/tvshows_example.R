# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

nDinners = 20
tvshows = c('Gordon', 'Jamie', 'Julia', 'Nigella')
nTvshows = length(tvshows) 

tvshowDist = rep(1/nTvshows, nTvshows)

pseudoSuccesses = matrix(0, nTvshows, 2)
pseudoSuccesses[1,] = c(5,1)
pseudoSuccesses[2,] = c(3,4)
pseudoSuccesses[3,] = c(1,5)
pseudoSuccesses[4,] = c(1,7)

theta = rep(0L, nTvshows) # probability of a successful dinner for each tv show

for (j in 1:nTvshows) {
  a <- pseudoSuccesses[j,1]
  b <- pseudoSuccesses[j,2]
  prob_success_j <- rbeta(1, a, b)
  theta[j] <- prob_success_j
}


nDinners <- 100
z <- rep(NA, nDinners)              # which show inspired the dinner?
x <- rep(NA, nDinners)   # was the dinner a success?

for (i in 1:nDinners) {
  z[i] <- sample(1:nTvshows, size=1, replace = T, tvshowDist)
  x[i] <- as.numeric(runif(1, min=0, max=1) < theta[z[i]])
}

for (j in 1:nTvshows) {
  print(paste0(tvshows[j], ' inspired ', sum(x[z==j]), '/ ', sum(z==j),' successful dinners.'))
}

### Now we use JAGS to recover theta!


library('rjags')
ms <- '
model {
  a <- 1
  b <- 1
  for (j in 1:nTvshows) {
    theta[j] ~ dbeta(a, b)
  }

  for (i in 1:nDinners) {
    x[i] ~ dbern(theta[z[i]])
  }
}
'
jags_dat <- list('x' = x, 'z' = z, 'nDinners' = nDinners, 'nTvshows' = nTvshows)
model <- jags.model(textConnection(ms), jags_dat)

params <- c('theta')
samples <- do.call('rbind', coda.samples(model, variable.names = params, n.iter = 5000))

par(mfrow=c(2,2))
for (j in 1:nTvshows) {
  hist(samples[,paste0('theta[', j, ']')], probability = T, breaks = 30, 
       main = paste0(tvshows[j], ' (', sum(z==j), ' dinner(s))'), 
       xlim = c(0, 1), xlab = expression(theta), ylab = paste0('p(', expression(theta), ' | z, x)'))
  abline(v = theta[j], col='red', lwd=3)
  mle = sum(x[z==j]) / sum(z==j)
  abline(v = mle, col='blue', lwd=3)
}
legend('bottomright', legend = c('True theta', 'MLE'), col = c('red', 'blue'), lty=c(1,1), lwd=c(3,3))
title('Posterior distributions of theta for each show', outer=T)


# why isn't the fit exactly around the true value?
# 1. Not enough data
# 2. Not enough samples

# Why is the fit wide?
# 1. Not enough data
# 2. Not enough samples (too a lesser degree)
# 3. Problem is difficult!