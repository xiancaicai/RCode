#Homework assignment (2021)
library(tidyverse)

############################################################ PART 1

#OLS linear regression function
linear_regression <- function(data, dep, indep, intercept = TRUE) { 
  y <- as.matrix(data[, dep])
  x <- as.matrix(data[, indep])
  
  if (intercept == TRUE) { 
    x <- cbind(1, x)
  }
  
  beta <- c(solve(crossprod(x)) %*% crossprod(x, y))
  fits <- x %*% beta
  resids <- y - fits
  sigma2 <- sum(resids^2)/(length(resids)-ncol(x))
  se <- sqrt(diag(sigma2 * solve(crossprod(x))))
  
  names(beta) <- colnames(x)
  
  return_obj <- list(beta = beta, se = se,
                     residuals = c(resids), fitted = c(fits),
                     sigma = sqrt(sigma2), dep = dep, indep = indep,
                     intercept = intercept, y = c(y))
  
  class(return_obj) <- "linear_regression"
  
  return(return_obj)
}


#Confidence interval function
ci <- function(lin_mod, parameter, alpha) {
  n = length(lin_mod$y) #number of observations
  p = length(lin_mod$indep) #number of estimated parameters
  
  #lower value
  upper <- lin_mod$beta[2] - qt(alpha/2, df = n - p) * lin_mod$se[2]  
  #upper value
  lower <- lin_mod$beta[2] + qt(alpha/2, df = n - p) * lin_mod$se[2]
  
  conf_level <- (1-alpha) * 100 #confidence level
  
  return_ci <- list(parameter, lower, upper, conf_level)
  class(return_ci) <- "linear_regression_ci"
  
  return(return_ci)
}

#Linear regression, output to be used in function ci()
lin_mod <- linear_regression(ggplot2::economics,
                             dep = "pce",
                             indep = c("unemploy", "pop", "psavert"))

#An example of the functon in use, with its output
ci_obj <- ci(lin_mod, "unemploy", 0.05)
ci_obj

print.linear_regression_ci <- function(input_obj){
  parameter <- input_obj[[1]]
  lower <- input_obj[[2]]
  upper <- input_obj[[3]]
  conf_level <- input_obj[[4]]
  
  return(cat("A" , conf_level, "% confidence interval for",
             deparse(substitute(parameter)), " is given by: \n(",
             lower, ",", upper,")."))
}

print(ci_obj)


############################################################ PART 2

#Function to compute a classical or stratified two-sample t-test.
test_strat <- function(data) {
  if(is.data.frame(data)) { 
    #Checking that format of input data is a data frame/tibble
    
    #Stratified two-sample t-test:
    if("strata" %in% names(data) & "x" %in% names(data) &
       "treatment" %in% names(data)){
      weight_denominator <- numeric()
      t_numerator <- numeric()
      t_denominator <- numeric()
      n <- length(unique(data$strata))
      
      #calculate weight denominator
      for(i in 1:n) {
        strat_subset <- subset(data, data$strata==i)
        
        n1k <- length(which(strat_subset$treatment==1))
        n2k <- length(which(strat_subset$treatment==2))
        
        weight_numerator_1 <- (n1k*n2k)/(n1k+n2k)
        
        weight_denominator <- cbind(weight_denominator, weight_numerator_1)
      }
      
      #calculate weight for stratum k
      for (k in 1:n) {
        strat_subset <- subset(data, data$strata==k)
        
        n1k <- length(which(strat_subset$treatment==1))
        n2k <- length(which(strat_subset$treatment==2))
        
        weight_numerator <- (n1k*n2k)/(n1k+n2k)
        weight_k <- weight_numerator/sum(weight_denominator)
        
        s1k2 <- var(strat_subset$x[strat_subset$treatment==1])
        s2k2 <- var(strat_subset$x[strat_subset$treatment==2])
        
        #calculate stratum sample variance
        sigma2_k <- ((n1k+n2k)/(n1k*n2k))*(((n1k-1)*s1k2+
                                              (n2k-1)*s2k2)/(n1k + n2k - 2))
        
        t_numerator <- cbind(t_numerator, 
                             weight_k*(mean(strat_subset$x[strat_subset$treatment==1]) - 
                                         mean(strat_subset$x[strat_subset$treatment==2])))
        
        weight2_sigma2 <- (weight_k^2)*sigma2_k
        t_denominator <- cbind(t_denominator, weight2_sigma2)
      }
      
      t_stat <- sum(t_numerator)/sqrt(sum(t_denominator))
      stratified <- TRUE
      
      return(tibble(t_stat, stratified))
    }
    
    #Classical two-sample t-test:
    else if("x" %in% names(data) & 
            "treatment" %in% names(data) &
            !("strata" %in% names(data)))
    {
      xbar_1 <- mean(data$x[data$treatment==1])
      xbar_2 <- mean(data$x[data$treatment==2])
      s2_1 <- var(data$x[data$treatment==1])
      s2_2 <- var(data$x[data$treatment==2])
      n1 <- length(data$x[data$treatment==1])
      n2 <- length(data$x[data$treatment==2])
      
      t_stat <- (xbar_1-xbar_2)/sqrt((s2_1/n1)+(s2_2/n2))
      stratified <- FALSE
      
      return(tibble(t_stat, stratified))
    }
    else {
      stop("Requested variables not included, please revise and try again.")
    } #when x, treatment not included in variable list. 
  }  
  else
  {
    stop("Input is not of right format, please revise and try again.")
  } #when input is not a data frame/tibble.
}


#simulated hypothetical data
set.seed(2021)
strat <- tibble(x = c(rnorm(200, 35), rnorm(200, 55), rnorm(200, 75)),
                treatment = rep(1:2, 300),
                strata = c(rep(1, 200), rep(2, 200), rep(3, 200)))

#Stratified two-sample t-test
test_strat(data = strat)

#Classical two-sample t-test
no_strat <- select(strat, -strata)
test_strat(no_strat)

#Output when input of wrong format is used
wrong_format <- as.matrix(strat)
test_strat(data = wrong_format)
