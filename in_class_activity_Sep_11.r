library(dplyr)
# n= 500 mean =5.5 median =5 Sd dev= 2.5
n = 500
mean = 5.5
median = 5
sd = 2.5
x = 5

ppois(x,lambda = mean)
ppois(q = x, lambda = mean)

pnorm(x,mean = mean, sd = 2.5)
pexp(x,rate = 1/mean)

rpois(n = n, lambda = mean)%>%hist()
rnorm(n = n, mean = 5.5,sd = 2.5)%>% hist()
rexp(n = n, rate = 1/mean)%>%hist()

f = function(t){ t }
f(1)

# function example:
add_two_numbers <- function(x, y) {
  return(x + y)}
result = add_two_numbers(4,6)
