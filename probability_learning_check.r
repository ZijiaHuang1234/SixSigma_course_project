library(dplyr)
library(ggplot2)
library(broom)
library(mosaicCalc)

# Randomly sample 500 visits from a poisson distribution with a mean of 5.5
visits <- rpois(n = 500, lambda = 5.5)
# Check out the distribution!
visits %>% hist()
#get the frequency for 5 visit in the distribution
pd5 <- dpois(5, lambda = 5.5)
pd5
#Approximate the PDF of our simulated visit
dism <- visits %>% density() %>% approxfun()

#cumulative probability(CDF)
cd5 <- ppois(q=5, lambda = 5.5)
cd5

#Quantiles 
q5 = qpois(p = c(0.25,0.75), lambda = 5.5)
q5

#rm
rm(visits, pd5,cd5,q5)

#Learning Check 
market_visits <- pnorm(5, mean = 5.5, sd = 2.5)
#probbability that customers will stop at the market for 5 time visits would be

# if the visit follow gama distribution what's its expected probability
shape <- 5.5^2 /2.5^2
scale <- 2.5^2/5.5
rate <- 1/scale
1 - pgamma(5,shape = shape,rate =rate)
# if the visists follow the Exponential Distribution
exp_rate <- 1/5.5
1 - pexp(5,rate =exp_rate)



##############EXERCISE 3
