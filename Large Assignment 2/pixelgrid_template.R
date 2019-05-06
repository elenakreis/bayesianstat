# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())

# Helper functions
number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  binary_vector = binary_vector[-(1:(length(binary_vector) - noBits))]
  binary_vector[binary_vector==0] = -1
  return(binary_vector)
}

plot_evidences = function(evidences, sortBy) {
  sorted = order(evidences[,sortBy], decreasing = T)
  matplot(evidences[sorted,], type = 'b', lty = rep(1,nmodels), pch=20, col = 1:4, xlab = 'Dataset number', ylab = 'Evidence')
  legend("topright", legend = c('Model 0', 'Model 1', 'Model 2', 'Model 3'), col=1:4, pch=20)
  return(sorted)
}

# Helper function to select correct likelihood function
compute_likelihood = function(model_id, weights, y, x){
  likelihood = 1
  for(i in 1:9){
    if(model_id == 2) # Index for model 1
      likelihood = likelihood * (1/(1 + exp(-y[i]*weights*x[i,1])))
    if(model_id == 3) # Index for model 2
      likelihood = likelihood * (1/(1 + exp(-y[i]*(weights[1]*x[i,1] + weights[2]*x[i,2]))))
    if(model_id == 4) # Index for model 3
      likelihood = likelihood * (1/(1 + exp(-y[i]*(weights[1] + weights[2]*x[i,1] + weights[3]*x[i,2]))))
  }
  return(likelihood)
}


# Create all the possible data sets
npixels = 9
ndatasets = 2^npixels
nmodels = 4

datasets = matrix(0, nrow = ndatasets, ncol = npixels)

for (d in 1:ndatasets) {
  datasets[d,] = number2binary(d, npixels)
}


x = matrix(0, npixels, 2)
x[1,] = c(-1, 1)
x[2,] = c(0, 1)
x[3,] = c(1, 1)
x[4,] = c(-1, 0)
x[5,] = c(0, 0)
x[6,] = c(1, 0)
x[7,] = c(-1, -1)
x[8,] = c(0, -1)
x[9,] = c(1, -1)


# Compute the evidences for each data set
evidences = matrix(0, nrow = ndatasets, ncol = nmodels)
evidences[,1] = 1/512

# Parameters for the prior weights
mu = 0
sigma = 10

# The number of samples to get
nsamples = 10000


for (d in 1:ndatasets) {
  y = datasets[d,]
  # compute evidence using Monte Carlo

  for (i in 2:nmodels) { # Start with 2 because we already have the evidence for model 1
    evidence = 0
    for (s in 1:nsamples) {
      # get weights from prior
      w = rnorm(i-1, mu, sigma)
      pDwm = compute_likelihood(i, w, y, x)
      # print(pDwm)
      evidence = evidence + pDwm
    }
    # print(evidence)
    evidences[d,i] = evidence/nsamples
  }
}

# 2.3
sorted_order = plot_evidences(evidences, sortBy=4)
print(datasets[511,])
print(datasets[512,])


