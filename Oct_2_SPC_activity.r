# Import pacakges
library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)

# read_dataset
water = read_csv("sysen/workshops/onsen.csv")
head(water)
ggprocess(x = water$time, y = water$temp, xlab = "Subgroup", ylab = "Metric")
#Calculate the Average 

#EXERCISE 3:
library(dplyr)
library(readr)
library(ggplot2)
water = read_csv("sysen/workshops/onsen.csv")
water %>% 
  summarize(sigma_t = sd(temp))
tibble(rep = 1:1000) %>%
  group_by(rep) %>%
  reframe(water) %>%
  group_by(rep) %>%
  sample_n(size = n(),replace = TRUE)

boot %>%
  group_by(rep) %>%
  summarise(sigma_t = sd(temp))
statsigma_t = 