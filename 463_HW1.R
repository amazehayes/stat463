## Stat 463 HW1 - Addison Hayes

# Problem 9
install.packages("quantmod")
library(quantmod)
getSymbols("MSFT", from = '2017-09-01')
msft = MSFT[, "MSFT.Close"]

# Part a
plot(msft)

# Part b
# The postulated model of Xt = Xo + Xt-1 + Wt appears to hold for this time series.
# In a Gaussian random walk, the random variable after a random walk can be 
# expressed as the cumulated sum of all the RVs that precede it.
# In this tme series, the assumptions appear to hold given the nature of the plot
# and a great example of a Gaussian random walk is finance and stock prices.

# Part c
# A suitable estimator for Xo would be the starting point of our data
min(msft) # estimator for Xo

# A suitable estimator for sigma2 would be the standard deviation squared of the series
sd(msft)^2 # estimate of sigma squared




# Problem 10
install.packages(c("RcppArmadillo","devtools","knitr","rmarkdown"))
devtools::install_github("SMAC-Group/wv")
devtools::install_github("SMAC-Group/simts")
library(simts)

# Parameters
N = 1000
mu = 0.5
phi = 0.25
sigma2 = 1.5

# Define the model
model = AR1(phi = phi, sigma2 = sigma2)

# Simulate time series
set.seed(123)
Xt = mu + gen_gts(N, model)






