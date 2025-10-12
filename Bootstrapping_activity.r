library(tidyverse)

#read the data
water <- read.csv("D:/codeWork/5300_repository/workshops/onsen.csv")
xbar <- mean(water$ph, na.rm = TRUE)

# Bootstrapping process
set.seed(123)
myboot <- tibble(rep = 1:1000) %>%
  group_by(rep) %>%
  mutate(
    sample = list(sample_n(water, size = nrow(water), replace = TRUE))
  ) %>%
  mutate(
    mean_ph = map_dbl(sample, ~ mean(.x$ph, na.rm = TRUE))
  ) %>%
  ungroup() 

glimpse(myboot)

#get the result
my_boot_result <- myboot %>%
  summarize(
    boot_mean = mean(mean_ph),
    se        = sd(mean_ph),
    lower     = quantile(mean_ph, probs = 0.025),
    upper     = quantile(mean_ph, probs = 0.975)
  )

my_boot_result
