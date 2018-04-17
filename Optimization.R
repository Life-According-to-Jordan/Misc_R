#This script is incomplete 

#load packages 
library(nloptr)

#set seed for reproducability 
set.seed(100)

#mainy for terminal use 
print("Generating Matrix...")

#set up data 
N=100000
K=10
X<-matrix(rnorm(N*K,mean=0,sd=1), N, K)
X[,1] <- 1 
#X<-cbind(X[,1, drop=FALSE], 1, X[,2:10] ) 
#X<-cbind( X, 1) [ ,c(1,6,2:5) ] 

#set parameters 
eps = rnorm(N, mean = 0, sd = 0.5)

#view head of data to make sure it looks nice 
head(eps)

#set up variables 
B <- c(1.5, -1, 0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
Y <- X %*%  B + eps

#view head of data to make sure it looks nice 
head(Y)

#regression of Y on X 
estimates <- lm(Y~X -1)

#show estimates of regression 
summary(estimates)

##################################
# Need to finish gradient descent 
###################################

#stepsize <- 0.0000003
gradient <- function(theta,Y,X) {
  grad <- as.vector(rep(0,length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y - X%*%B)/(sig^2) grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y-X%*%B)/(sig^3)
  return ( grad ) }
