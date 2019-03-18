library(ggplot2)
#2.1
a <- function(w,k){
  a = w*(k-2)+1
  return(a)
}
b <- function(w,k){
  b = (1-w)*(k-2)+1
  return(b)
}

N = 12
z = 8

w1 = 0.25
w2 = 0.75

k = 8

a1 = a(w1,k)
b1 = b(w1,k)

a2 = a(w2,k)
b2 = b(w2,k)


theta1 = seq(0,1,len=100)
plot1 = plot(theta1, dbeta(theta1, a1, b1), type='l')

theta2 = seq(0,1,len=100)
plot2 = plot(theta2, dbeta(theta2, a2, b2), type='l')

#2.2
priorm1 = 0.5
priorm2 = 0.5

pD = function(z,N,a,b) { exp( lbeta(z+a,N-z+b) - lbeta(a,b) ) } # should we use this one?

pDm1 = pD(z,N,a1,b1)
pDm2 = pD(z,N,a2,b2)

bayesFactor = pDm1/pDm2

probratio = bayesFactor*(priorm1/priorm2)
postm1 = probratio/(1+probratio)
postm2 = 1-postm1


#2.3
k = 180

a1 = a(w1,k)
b1 = b(w1,k)

a2 = a(w2,k)
b2 = b(w2,k)

pD = function(z,N,a,b) { exp( lbeta(z+a,N-z+b) - lbeta(a,b) ) } # should we use this one?

pDm1 = pD(z,N,a1,b1)
pDm2 = pD(z,N,a2,b2)

bayesFactor = pDm1/pDm2

probratio = bayesFactor*(priorm1/priorm2)
postm1 = probratio/(1+probratio)
postm2 = 1-postm1







