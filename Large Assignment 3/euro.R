#1.1

N = 250
z = 110
theta = 0.5
lowTailZ = 0:z
sum(choose(N, lowTailZ) * theta^lowTailZ * (1-theta)^(N-lowTailZ) ) *2
#0.06642115


#1.2

#Dr. Bright took 250 flips as the implemented stopping procedure, this affecting the probabilities of the data.

#1.3

#spike_functon <- function(theta) {
#  if (theta == 0.5 ){
#    return (1)
#  } else{ 
#    return (0)
#  }
#}


#beta_prior <- function(theta, a = 1, b= 1){
#  return ((theta^(a-1) * (1 - theta)^(b-1))/beta(a,b))
#}

theta_null = 0.5
a = 1
b = 1

evidence_m0 <- function(N, z, theta_null = 0.5){ 
  return (theta_null^z * (1 - theta_null)^(N-z))
}


evidence_m1 <- function(a, b, N, z){
return (beta(a + z, N - z + b)/beta(a, b))
}

bf_10 <- evidence_m1(a, b, N, z)/evidence_m0(N, z)

#bf = 0.4767 -> model 0 is almost two times more likely to explain the data than model 1


#1.4

alpha <- 1:400
BF <- function(alpha, N, z){
  return (evidence_m1(alpha, alpha, N, z)/evidence_m0(N, z))
}

plot(alpha, BF(alpha, N, z), type = 'l')
abline(alpha, rep(0, 501))

#max bayes factor
alpha <- 1:150
alpha_max <- BF(alpha, N, z)
max_value <- max(alpha_max)
max_alpha <- which(alpha_max == max(alpha_max))

#Maximum BF value is 1.940734 at an alpha value of 48. 

#1.5

# The line seems to approximate 1, which would mean that the difference between the two models will diminish until it completely disappears.

#1.6

# theta^z * (1-theta)^(N-z)

#1.7

evidence_m2 <- function(N, z, theta_null2 = z/N){ #because the initial value of z we started working with was 110, then we also modified the value of theta here 
  
  return (theta_null2^z * (1 - theta_null2)^(N-z))
  
}

bf_20 <- evidence_m2(N, z)/evidence_m0(N, z)

#6.0759

#1.8
#By looking at the table, one can see that a Bayes Factor of over 6 means a strong effect (it's very likely for the first model to
#explain the data way better than the second one). This supports Dr. Blight's statement that the coin is not fair.


#1.9

z1 = 109
lowTailZ1 = 0:z1
sum(choose(N, lowTailZ1) * theta^lowTailZ1 * (1-theta)^(N-lowTailZ1) ) *2
#0.049 -> result is statistically significant 



#1.10

alpha <- 1:390 
BF <- function(N , z1, alpha){return(evidence_m1(alpha, alpha, N, z1) / evidence_m0(N, z1))}
plot(alpha, BF(N, z1, alpha), type = 'l')
abline(alpha, rep(0, 501))

alpha <- 1:100
max_BF <- BF(N, z=109, alpha)
max_value <- max(max_BF)
max_alpha <- which(max_BF == max(max_BF))

#The maximum possible Bayes Factor this time is 2.3344, for an alpha of 40.

