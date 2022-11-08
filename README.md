# RCode
Some projects, tasks and tests written in R. Both uni projects and personal projects included.


## :pushpin: Semi-supervised Hidden Markov Models for POS tagging [2022]
**Code:** [HMM_POS.R](https://github.com/xiancaicai/RCode/blob/main/HMM_POS.R) and [HMM_Viterbi.stan](https://github.com/xiancaicai/RCode/blob/main/HMM_Viterbi.stan)

**Packages used: rstan, MCMCpack, loo.**

Studied Hidden Markov Models (HMM) and used them in Part-of-speech (POS) tagging, for a Bayesian project. In summary, the HMM is a probabilistic model that can be used to model hidden non-observable states by observing past sequences. Here, sequences of words are observed, where their respective POS category (such as verb, adjective, etc.) are seen as hidden latent states. Given a new sentence with an unobserved state sequence, the models predict the most probable hidden state POS sequence using the Viterbi algorithm.

## :pushpin: Supervised Learning with Multilayer Neural Networks [2021]
**Code:** [ChineseMNIST_NN.R](https://github.com/xiancaicai/RCode/blob/main/ChineseMNIST_NN.R)

**Packages used: Keras, Magick, Tidyverse.**

<img src="chinesemnist.png" height="200">

Compared optimization algorithms used in classification with multilayer neural networks (nonlinear statistical models). The models were trained to classify Chinese number characters (a subset of the 'Chinese MNIST' data set from the University of Newcastle available [here](https://data.ncl.ac.uk/articles/dataset/Handwritten_Chinese_Numbers/10280831/1)) using three different optimization algorithms: classic Stochastic Gradient Descent (SGD), ADAM and RMSprop. 

## :pushpin: Linear Regression, Confidence Intervals and t-Tests [2021]
**Code:** [LinReg_CI_tTest.R](https://github.com/xiancaicai/RCode/blob/main/LinReg_CI_tTest.R)

Without external packages wrote functions which execute **linear regression**, compute **confidence intervals** and **t-Tests (both classical and stratified)**, to understand how the math works behind the built-in functions so-often used. Homework coded with reference to course material.

## :pushpin: Frequentist vs Bayesian estimation [2021]
**Code:** [ResDev_IRLS_Boots_MCMC.R](https://github.com/xiancaicai/RCode/blob/main/ResDev_IRLS_Boots_MCMC.R)

Without external packages wrote functions to calculate **Residual Deviance**, perform **Iteratively Reweighted Least Squares**, **Monte Carlo Metropolis Hastings** simulations, **Bootstrap** sampling and estimation. Homework coded with reference to course material.

