library(nloptr)
set.seed(100)
print("Generating Matrix...")
N=100000
K=10
X<-matrix(rnorm(N*K,mean=0,sd=1), N, K)
X[,1] <- 1 
#X<-cbind(X[,1, drop=FALSE], 1, X[,2:10] ) 
#X<-cbind( X, 1) [ ,c(1,6,2:5) ] 
eps = rnorm(N, mean = 0, sd = 0.5)
head(eps)
B <- c(1.5, -1, 0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
Y <- X %*%  B + eps
head(Y)
estimates <- lm(Y~X -1)
summary(estimates)
print("Question 5")


#stepsize <- 0.0000003
gradient <- function(theta,Y,X) {
  grad <- as.vector(rep(0,length(theta)))
  beta <- theta[1:(length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y - X%*%beta)/(sig^2) grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y-X%*%beta)/(sig^3)
  return ( grad ) }
