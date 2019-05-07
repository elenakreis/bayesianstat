# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())
setwd("C:/Users/Mihaela/Desktop/Uni/B2/Sem 2/Bayesian Statistics/DBDA2Eprograms/DBDA2Eprograms")

# Required packages for this exercise.
require(rjags)
require(coda)
require(ggplot2)
require(gridExtra)
require(tidyr)


rm(list=ls())
require(rjags)
require(coda)

load(file = "gameofregression.Rdata")


n = length(y)
p = ncol(x)
y[length(y)] <- NA 
x[length(y),] <- c(1, 0, 5, 184, 20)



linear_regression ="
model {
# Prior

w0 ~ dnorm(0, 1.0)
for(j in 1:p){
w[j] ~ dnorm(0, 1.0)
}

tau ~ dgamma(0.01, 0.01)


  # Likelihood

for(i in 1:n){
mu[i] = w0 + t(w[1:p])%*%x[i, ]
y[i] ~ dnorm(mu[i], tau)

}
}

"

niter = 10000
nchains = 4
data = list('x' = x,
            'y' = y,
            'n' = n,
            'p' = p
            ) # to add by yourself

jagsmodel <- jags.model(textConnection(linear_regression), 
                        data = data,
                        n.chains = nchains)

samples = coda.samples(jagsmodel, c('w0', 'w', 'y'), n.iter = niter) # to add by yourself


samples_df = data.frame(w0 = samples[[1]][,'w0'],
                        w1 = samples[[1]][,'w[1]'],
                        w2 = samples[[1]][,'w[2]'],
                        w3 = samples[[1]][,'w[3]'],
                        w4 = samples[[1]][,'w[4]'],
                        w5 = samples[[1]][,'w[5]'])
colnames(samples_df) = c('w0','w1','w2','w3','w4','w5') 

# Creating plots
w0_plot <- ggplot(samples_df, aes(w0)) + 
  geom_histogram(bins = 50) 
w1_plot <- ggplot(samples_df, aes(w1)) + 
  geom_histogram(bins = 50) +
  scale_x_continuous(limits = c(-1,1)) 
w2_plot <- ggplot(samples_df, aes(w2)) + 
  geom_histogram(bins = 50)+
  scale_x_continuous(limits = c(-1,1)) 
w3_plot <- ggplot(samples_df, aes(w3)) + 
  geom_histogram(bins = 50)+
  scale_x_continuous(limits = c(-1,1))
w4_plot <- ggplot(samples_df, aes(w4)) + 
  geom_histogram(bins = 50)+
  scale_x_continuous(limits = c(-1,1)) 

w5_plot <- ggplot(samples_df, aes(w5)) + 
  geom_histogram(bins = 50)+
  scale_x_continuous(limits = c(-1,1)) 


# Plotting neatly
grid.arrange(w0_plot, w1_plot, w2_plot, w3_plot, w4_plot, w5_plot, ncol=2)
summary(samples)


