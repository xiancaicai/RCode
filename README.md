# RCode
For some projects, tasks and tests written in R. Both uni projects and personal projects included.

## Linear Regression, Confidence Intervals and t-Tests
**Code:** [2021_LinReg_CI_tTest.R](https://github.com/xiancaicai/RCode/blob/main/2021_LinReg_CI_tTest.R)

Functions which execute linear regression, compute confidence intervals and t-Tests (both classical and stratified).
(To understand how the actual math works behind the built-in functions so-often used.)

## Frequentist vs Bayesian estimation
**Code:** [2021_ResDev_IRLS_Boots_MCMC.R](https://github.com/xiancaicai/RCode/blob/main/2021_ResDev_IRLS_Boots_MCMC.R)

Coded functions to calculate **Residual Deviance**,
perform **Iteratively Reweighted Least Squares**,
**Monte Carlo Metropolis Hastings** simulations,
**Bootstrap** sampling and estimation.

## Supervised Learning (Classification) with Multilayer Neural Networks
**Code:** [2021_ChineseMNIST_NN.R](https://github.com/xiancaicai/RCode/blob/main/2021_ChineseMNIST_NN.R)

**Packages used: Keras, Magick, Tidyverse.**

<img src="chinesemnist.png" height="200">

Compared optimization algorithms used in classification with multilayer neural networks (nonlinear statistical models). The models were trained to classify Chinese number characters (a subset of the 'Chinese MNIST' data set from the University of Newcastle available [here](https://data.ncl.ac.uk/articles/dataset/Handwritten_Chinese_Numbers/10280831/1)) using three different optimization algorithms: classic Stochastic Gradient Descent (SGD), ADAM and RMSprop. 
