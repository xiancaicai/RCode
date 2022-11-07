#Semi-Supervised Hidden Markov Model used for Part-of-speech (POS) tagging
#Project coded in Stan and R for a course in Bayesian statistics.
#Stan code largely built via examples in the Stan User's Guide.
#See separate file: HMM_Viterbi.stan

#Packages
library(rstan)
library(MCMCpack)
library(LaplacesDemon)
library(readxl)
library(loo)

########################################################## PREPARE DATA
df <- read_excel("NER_new.xlsx") #train
df_new <- read_excel("w_new.xlsx") #test

# DATA
w = as.integer(as.factor(df$Word)) #words/tokens
z = as.integer(as.factor(df$pos_4)) #categories/states
w_new = as.integer(df_new$w_new) #observed words in sentence

# CONSTANTS
K = length(unique(df$pos_4)) #num_categories
V = length(unique(df$Word)) #num_words
T_ = length(df$Word) #num_instances
T_unsup = length(w_new)

#Model 1 PRIOR 
alpha1 = rep(0.1,K) #transit_prior
beta1 = rep(0.1,V) #emit_prior

#Model 2 PRIOR 
alpha2 = rep(0.8,K) 
beta2 = rep(0.8,V) 

#Model 3 PRIOR 
alpha3 = rep(2,K) 
beta3 = rep(2,V) 

########################################################## RUN MODELS

#Number of iterations
iter_n = 3000
warmup_n = 1500

#MODEL 1
# Run stan model and extract draws
# Compile model
set.seed(1234)
fit1 <- stan(file='HMMViterbi.stan', cores=8,
             data=list(K=K,V=V,T_=T_,
                       T_unsup=T_unsup,w=w,z=z,w_new=w_new,
                       alpha=alpha1,beta=beta1), 
             iter=iter_n, warmup = warmup_n, chains = 4, 
             control = list(adapt_delta=0.98))
draws1 <- rstan::extract(fit1)

#MODEL 2
set.seed(1234)
fit2 <- stan(file='HMMViterbi.stan', cores=8,
             data=list(K=K,V=V,T_=T_,
                       T_unsup=T_unsup,w=w,z=z,w_new=w_new,
                       alpha=alpha2,beta=beta2), 
             iter=iter_n, warmup = warmup_n, chains = 4, 
             control = list(adapt_delta=0.98))
draws2 <- rstan::extract(fit2)

#MODEL 3
set.seed(1234)
fit3 <- stan(file='HMMViterbi.stan', cores=8,
             data=list(K=K,V=V,T_=T_,
                       T_unsup=T_unsup,w=w,z=z,w_new=w_new,
                       alpha=alpha3,beta=beta3), 
             iter=iter_n, warmup = warmup_n, chains = 4, 
             control = list(adapt_delta=0.98))
draws3 <- rstan::extract(fit3)

########################################################## MODEL EVALUATION

# Functions to be used
mcsequence <- function(x, drop = FALSE) { #most common sequence
  xx <- do.call("paste", c(data.frame(x), sep = "\r"))
  tx <- table(xx)
  mx <- names(tx)[which(tx == max(tx))[1]]
  as.vector(x[match(mx, xx), , drop = drop])
}

correct  <- function(pos_predicted_and_correct){
  # Function to calculate how many POS correctly predicted
  N <- nrow(pos_predicted_and_correct)
  count = matrix(NA, nrow(pos_predicted_and_correct), 1)
  for (i in 1:N){
    if (pos_predicted_and_correct[i,1]==pos_predicted_and_correct[i,2]){
      count[i,1] = 1
    }else{
      count[i,1] = 0
    }
    list= list(correct = sum(count),
               total = N,
               percent_correct =sum(count)/N)
  }
  return(list)
}

# Evaluation - Accuracy (percentage)
correct_pos <- as.integer(df_new$z_new)

#Model 1
predicted_pos_m1 <- mcsequence(draws1$z_star)
pos_predicted_and_correct_m1 <- cbind(predicted_pos_m1,correct_pos)   
correct(pos_predicted_and_correct_m1) #nr correct, total, percentage

#Model 2
predicted_pos_m2 <- mcsequence(draws2$z_star)
pos_predicted_and_correct_m2 <- cbind(predicted_pos_m2,correct_pos)   
correct(pos_predicted_and_correct_m2) #nr correct, total, percentage

#Model 3
predicted_pos_m3 <- mcsequence(draws3$z_star)
pos_predicted_and_correct_m3 <- cbind(predicted_pos_m3,correct_pos)   
correct(pos_predicted_and_correct_m3) #nr correct, total, percentage

# Evaluation - PSIS loo-CV
# Model 1
log_lik_1 <- extract_log_lik(fit1, merge_chains = FALSE)
r_eff_1 <- relative_eff(exp(log_lik_1), cores = 8)
loo_1 <- loo(log_lik_1, r_eff = r_eff_1, cores = 8)

# Model 2
log_lik_2 <- extract_log_lik(fit2, merge_chains = FALSE)
r_eff_2 <- relative_eff(exp(log_lik_2), cores = 8)
loo_2 <- loo(log_lik_2, r_eff = r_eff_2, cores = 8)

# Model 3
log_lik_3 <- extract_log_lik(fit3, merge_chains = FALSE)
r_eff_3 <- relative_eff(exp(log_lik_3), cores = 8)
loo_3 <- loo(log_lik_3, r_eff = r_eff_3, cores = 8)

#Comparison
loo_compare(loo_1, loo_2, loo_3)

# EVvalution - Rhat and n_neff
max_Rhat1 <- max(monitor(fit1)$Rhat,na.rm=TRUE)
max_Rhat2 <- max(monitor(fit2)$Rhat,na.rm=TRUE)
max_Rhat3 <- max(monitor(fit3)$Rhat,na.rm=TRUE)

min_n_eff1 <- min(monitor(fit1)$n_eff,na.rm=TRUE)
min_n_eff2 <- min(monitor(fit2)$n_eff,na.rm=TRUE)
min_n_eff3 <- min(monitor(fit3)$n_eff,na.rm=TRUE)
