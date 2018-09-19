## Stat 463 HW1 - Addison Hayes
#6 AR(1)
library(simts)
n = 1000
phi = -.85
sig_sq = 1

Xt = gen_gts(n, AR1(phi = phi, sigma2 = sig_sq))
plot(Xt)

# MA(1)

n = 1000                       
sig_sq = 2                          
theta = 0.9                          
Xt = gen_gts(n, MA1(theta = theta, sigma2 = sig_sq))
plot(Xt)

# Drift Combined

n = 1000                              
omega = 0.1                         
sig_sq = 1                          
                        
model = WN(sigma2 = sig_sq) + DR(omega = omega)
Xt = gen_lts(n = n, model = model)
plot(Xt)

# RW
n = 1000                               
gamma2 = 1                             
Xt = gen_gts(n, RW(gamma2 = gamma2))
plot(Xt)

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


#James 10
# Load library
library(simts)

# Parameters
N = 1000
mu = 0.5
phi = 0.25
sigma2 = 1.5

# Define the model
model = AR1(phi = phi, sigma2 = sigma2)

# Simulate time series
Xt = mu + gen_gts(N, model)

head(Xt)


B = 500

result = matrix(NA,B)
delthat = matrix(NA,B)

set.seed(234)
for (i in seq_len(B)){
  
  Xt = mu + gen_gts(N, model)
  result[i,] = var(Xt)
  delthat[i,] = result[i,]/mu
}

alpha = .05
quantile(delthat, c(alpha/2, 1 - alpha/2))



