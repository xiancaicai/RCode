# RCode
For some projects, tasks and tests written in R. Both uni projects and personal projects included.

## Linear Regression, Confidence Intervals and t-Tests
**Code:** [2021_LinReg_CI_tTest.R](https://github.com/xiancaicai/RCode/blob/main/2021_LinReg_CI_tTest.R)

Functions which execute linear regression, compute confidence intervals and t-Tests (both classical and stratified).
(To understand how the actual math works behind the built-in functions so-often used.)

## Frequentist vs Bayesian estimation
**Code:** [2021_ResDev_IRLS_Boots_MCMC.R](https://github.com/xiancaicai/RCode/blob/main/2021_ResDev_IRLS_Boots_MCMC.R)

Functions to calculate Residual Deviance,
perform Iteratively Reweighted Least Squares,
Monte Carlo Metropolis Hastings simulations,
Bootstrap sampling and estimation. (To compare Frequentist and Bayesian estimation.)

## Supervised Learning (Classification) with Multilayer Neural Networks (using Chinese MNIST)
**Code:** [2021_ChineseMNIST_NN.R](https://github.com/xiancaicai/RCode/blob/main/2021_ChineseMNIST_NN.R)

![Chinese Number Characters](chinesemnist.png)
<img src="chinesemnist.png" width="100" height="100">

Packages used: Keras, Magick, Tidyverse.

To compare optimization algorithms used in classification with nonlinear statistical models known as neural networks, multilayer
neural networks were trained to classify Chinese number characters (a subset of the 'Chinese MNIST' data set from the University of Newcastle available [here](https://data.ncl.ac.uk/articles/dataset/Handwritten_Chinese_Numbers/10280831/1)). Three different optimization algorithms were tested: classic Stochastic Gradient Descent (SGD), ADAM and RMSprop. 
