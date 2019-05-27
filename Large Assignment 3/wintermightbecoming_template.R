# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

# Required packages for this exercise.
require(rjags)
require(coda)
# --> fix path for your own system: source("R/DBDA2Eprograms/DBDA2E-utilities.R")

sW = 277
nW = 412

sE = 2545
nE = 4241

winter_model = "
model{
  
}
"

niter=10000
nchains=4
nsamples = niter*nchains

data <- list('sE' = sE, 'sW' = sW, 'nE' = nE, 'nW' = nW) # to be passed on to JAGS

parameters <- c() # fill in!

jagsmodel_winter <- jags.model(textConnection(winter_model), 
                               data = data, 
                               n.chains = nchains)

samples_winter = coda.samples(jagsmodel_winter, parameters, n.iter = niter)
samples = as.matrix(samples_winter)

samples.post = # fill in!
samples.prior =  # fill in




nbreaks = 30

histogram1 <- hist(samples.post, breaks=nbreaks, plot=F)   
histogram2 <- hist(samples.prior, breaks=nbreaks, plot=F)  

binwidth1 = histogram1$breaks[2] - histogram1$breaks[1] # ***
binwidth2 = histogram2$breaks[2] - histogram2$breaks[1]


histogram1$counts = (histogram1$counts / nsamples) / binwidth1 # *** 
histogram2$counts = (histogram2$counts / nsamples) / binwidth2


# Plot the histograms
plot( histogram1, col=rgb(0,0,1,1/2), xlim=c(-1,1), xlab = expression(delta), ylab = 'Probability density') 
plot( histogram2, col=rgb(1,0,0,1/2), xlim=c(-1,1), add=T) 

# Comment out the next line once you've installed this package
install.packages('polspline')
library(polspline) 
fit.posterior <- logspline(samples.post, lbound = -1, ubound = 1)
fit.prior <- logspline(samples.prior, lbound = -1, ubound = 1)
plot(fit.posterior, xlim = c(-1,1), add=T)
plot(fit.prior, xlim=c(-1,1), add=T)


posterior_at_0 <- dlogspline() # fill in
prior_at_0     <- dlogspline() # fill in

par(mfrow=c(1,2))
# Normal plot
plot(fit.posterior, xlim=c(), xlab = expression(delta), ylab = 'Probability density') # set the correct xlim (x axis limits)
plot(fit.prior, add = T, lty = 2)
# Add plotting of the circles at (delta, p(delta=0)). Hint: look up 'pch' for plotting in R
title('Full distributions')
legend(x = 'topleft', 1.9, c('Posterior', 'Prior'), lty=c(1,2))

# Zoomed plot
plot(fit.posterior, xlab = expression(delta), ylab = 'Probability density') # Set both xlim and ylim to zoom in.
plot(fit.prior, add = T, lty = 2) # You need to set the same xlim here for the plot to display properly.
# Add plotting of the circles at (delta, p(delta=0)). Hint: look up 'pch' for plotting in R
title('Zoomed plot')
legend(x = 'topleft', 1.9, c('Posterior', 'Prior'), lty=c(1,2))


# Compute Savage-Dickey ratios:


# Compute analytical Bayes factor:


# Compute relevant quantities for report:
