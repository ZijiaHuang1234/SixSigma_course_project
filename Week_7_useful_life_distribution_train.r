library(tidyverse)
library(mosaicCalc)
# import data
masks = read_csv("masks.csv")
masks %>% glimpse()

#get the mttf and lambda for the collumn of left_ear_loop
stat = masks %>%
summarize(
    mttf = mean(left_earloop),lambda =  1/mttf
)
prob = pexp(stat$mttf,rate = stat$lambda)
# use another way to calculate the mttf
#reliability function for exponential distribution,create a function for MTTF and r
r = function(t,lambda){exp(-1*t*lambda)}
mttf = antiD(tilde = r(t,lambda) ~t)
#example of using mttf function
mttf(t =1000,lambda = stat$lambda)

stat <- masks %>%
  summarize(
    # The literal mean time to fail 
    # in our observed distribution is this
    mttf = mean(left_earloop),
    # And lambda is this...
    lambda = 1 / mttf,
    # The observed median is this....
    median = median(left_earloop),
    # But if we assume it's an exponential distribution
    # and calculate the median from lambda,
    # we get t50, which is very close.
    t50 = log(2) / lambda)

# Check it out!
stat

#modal time to fail
# Create a separate analysis for modal time
lambda_val <- 1/mean(masks$left_earloop)
t_values <- 1:max(masks$left_earloop)
prob_values <- dexp(t_values, rate = lambda_val)

modal_analysis <- data.frame(
  t = t_values,
  prob = prob_values
) %>% 
  arrange(desc(prob)) %>% 
  head(3)
modal_analysis

#Learning_check 1
f = function(t,lambda){1- exp(-1*t*lambda)} # failure rate function is 1- exp(-1*t*lambda)
q1_answer = f(t = 20,lambda = 0.08)
q2_answer = f(t =20,lambda = 0.08)^2

#Learning check：
compare <- masks %>%
  summarize(
    mttf_right = mean(right_earloop),
    mttf_left =  mean(left_earloop),
    lambda_right = 1/mttf_right,
    lambda_left = 1/mttf_left
)

#19.3 Quantile of interest(continued)
r = function(t,lambda){exp(-1*t*lambda)}
r(t=10 + 5,lambda = stat$lambda)


