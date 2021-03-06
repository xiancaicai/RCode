# RCode
For some projects, tasks and tests written in R. Both uni projects and personal projects included.

## Linear Regression, Confidence Intervals and t-Tests
**Code:** [2021_LinReg_CI_tTest.R](https://github.com/xiancaicai/RCode/blob/main/2021_LinReg_CI_tTest.R)

Instead of using built-in functions or packages in R, I here have coded functions which execute linear regression, compute confidence intervals and t-Tests (both classical and stratified). This helps in understanding how the actual math works behind the built-in functions so-often used.

## Frequentist vs Bayesian estimation
**Code:** [2021_ResDev_IRLS_Boots_MCMC.R](https://github.com/xiancaicai/RCode/blob/main/2021_ResDev_IRLS_Boots_MCMC.R)

Here, functions have been built to calculate Residual Deviance, perform Iteratively Reweighted Least Squares, Monte Carlo Metropolis Hastings simulations, as well as Bootstrap sampling and estimation, to compare Frequentist and Bayesian estimation.

## Supervised Learning (Classification) with Multilayer Neural Networks (using Chinese MNIST)
**Code:** [2021_ChineseMNIST_NN.R](https://github.com/xiancaicai/RCode/blob/main/2021_ChineseMNIST_NN.R)

To compare optimization algorithms used in classification with nonlinear statistical models known as neural networks, we trained multilayer
neural networks to classify Chinese number characters (from the 'Chinese MNIST' data set), with three different optimization algorithms: classic Stochastic Gradient Descent (SGD), ADAM and RMSprop. Packages used: Keras, Magick, Tidyverse.
