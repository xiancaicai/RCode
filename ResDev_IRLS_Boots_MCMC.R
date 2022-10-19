#Homework assignment (2021)
library(tidyverse)

set.seed(8)
n  <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
lambda <- exp(2 + 0.8*x1 + 1.2*x2)
y <- rpois(n, lambda= lambda)
y <- cbind(y)
X <- cbind(x0 = 1, x1, x2)

df <- data.frame(y, X)
ggplot(data = df, mapping = aes(x = y)) +
  geom_histogram() #histogram of the generated data

############################################################ PART 1

#FREQUENTIST ESTIMATION
#Residual Deviance function
deviance = function(y, X, beta){
  #y - vector of dependent data
  #X - model matrix (1', x1', x2')'
  #beta - vector of regression coefficients
  n <- length(y)
  ratios <- numeric()
  tX <- t(X)
  for (i in 1:n){
    eta <- tX[,i] %*% beta 
    
    ylogy = ifelse(y[i] == 0, 0, y[i]*log(y[i]))
    ratio <- ylogy - y[i]*eta-(y[i]-exp(eta))
    ratios <- c(ratios, ratio)
    
  }
  result <- 2*sum(ratios)
  
  return(result)
}

#Iteratively Reweighted Least Squares function
irls = function(y, X, epsilon = 1e-6){
  #Finding the root(s) of the score function numerically
  #Estmating beta hat (MLE) via the Newton-Raphson method
  
  beta <- solve(t(X) %*% X) %*% t(X) %*% log(y + 0.5)
  D_current <- deviance(y, X, beta)
  D_prev <- 1e30
  i <- 0
  quotient <- (D_prev-D_current)/D_prev
  
  while(quotient >= epsilon) {
    i <- i + 1
    eta <- X %*% beta
    z <- X %*% beta + exp(-(X %*% beta)) * (y - exp(X %*% beta))
    matrix_diag <- as.vector(exp(eta))
    W <- diag(matrix_diag)
    beta <- solve((t(X) %*% W %*% X)) %*% t(X) %*% W %*% z
    D_prev <- D_current
    D_current <- deviance(y, X, beta)
    quotient <- (D_prev-D_current)/D_prev
    
  }
  
  return(beta)
}

irls(y, X, epsilon = 1e-6)

############################################################ PART 2

#BAYESIAN ESTIMATION
#With a Markov Chain Monte Carlo algorithm and Metropolis-Hastings Random Walk proposals are sampled from the posterior of beta.

#sig: the variance of the prior on beta
#eps: the variance of the random walk proposal
#N: size of the posterior sample

logPosterior = function(beta, y, X, sig){
  eta = c(X %*% beta)
  return(sum(y*eta - exp(eta) - 1/(sig^2)*c(t(beta) %*% beta)))
}

#Metropolis Hastings function
mhStep = function(beta, y, X, sig, eps){
  prop = beta + rnorm(length(beta), 0, eps^2)
  alpha = exp(logPosterior(prop, y, X, sig) - logPosterior(beta, y, X, sig))
  u = runif(1)
  # accept proposal with probability alpha
  if(u <= min(c(1, alpha))){
    return(prop)
  } else { 
    return(beta)
  } 
}

#Markov Chain Monte Carlo function
mcmc = function(y, X, sig, eps, N) {
  #MCMC reiterates the MH step to generate N samples from the posterior
  
  #for i in N:
  #sample beta_i by mhStep() using beta_(i-1)
  #and the proposal distribution
  
  p <- ncol(X)
  sp <- matrix(0, N, p) #empty matrix
  sp[1,] <- 1 #initial value
  
  for(i in 2:N){
    sp[i,] <- mhStep(sp[i-1,], y, X, sig, eps) 
  }
  return(sp) #matrix of size N x p, where p = size of vector beta
}

#the acceptance rate
# "n" the number of observations (n = 100)
acceptance <- function(betas, N){
  sumrate = 0
  for (i in 2:N){
    if (all(betas[i,] == betas[i-1,])){
      sumrate <- sumrate + 1
    }
  }
  return(sumrate/(N-1))
}

matrixbeta <- mcmc(y, X, 10, 0.08, 1000) #eps = 0.08
acceptance(matrixbeta, 1000)

#Bootstrap
#Comparing the two estimates (frequentist vs bayesian) via confidence intervals.

#Frequentist: non-parametric bootstrap.
#Generate N bootstrap estimates. (sampling with replacement)
#Estimate beta using bootstrap samples.
#Use this sample to estimate confidence interval.
bootstrap <- function(y, X, B, p){
  beta_freq <- matrix(0, B, p)
  for (i in 1:B){
    ids = sample(1:nrow(y), replace = T)
    X_bootstrap = X[ids,] #new bootstrap samples
    y_bootstrap = y[ids,]  
    
    # estimate beta using X_bootstrap, y_bootstrap
    beta_freq[i,] <- c(irls(y_bootstrap, X_bootstrap, epsilon = 1e-6))
  }
  return(beta_freq)
}

beta_freq <- bootstrap(y, X, 1000, 3) #generate bootstrap sample of 1000

#Comparing densities of bootstrap samples and posterior samples
df_bayes <- data.frame(beta1 = matrixbeta[,1],
                       beta2 = matrixbeta[,2],
                       beta3 = matrixbeta[,3])

df_freq <- data.frame(beta1 = beta_freq[,1],
                      beta2 = beta_freq[,2],
                      beta3 = beta_freq[,3])

#beta 1
ggplot() +
  geom_density(data = df_freq, aes(x=beta1), color="darkblue") + #Frequentist
  geom_density(data = df_bayes, aes(x=beta1), color= "red") + #Bayesian
  labs(x = 'Beta 1', y = 'Density')

#beta 2
ggplot() +
  geom_density(data = df_freq, aes(x=beta2), color="darkblue") + #Frequentist
  geom_density(data = df_bayes, aes(x=beta2), color= "red") + #Bayesian
  labs(x = 'Beta 2', y = 'Density')

#beta 3
ggplot() +
  geom_density(data = df_freq, aes(x=beta3), color="darkblue") + #Frequentist
  geom_density(data = df_bayes, aes(x=beta3), color= "red") + #Bayesian
  labs(x = 'Beta 3', y = 'Density')
