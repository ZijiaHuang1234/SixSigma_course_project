#read the csv
library(tidyverse)
library(dplyr)
products = read_csv("c:/codeWork/SixSigma/5300_repository/sysen/workshops/products.csv") %>% 
  group_by(subgroup) %>%
  summarize(grams = c(grams1, grams2, grams3, grams4, grams5)) %>%
  ungroup()
# get stats data
stat_s = products %>%
  group_by(subgroup) %>% 
  summarize(
xbar = mean(grams),
r= max(grams) - min(grams),
sd = sd(grams),
nw =  n(),
df = nw - 1
)%>% 
  # calculate short term sigma and 3 sigma limit
  mutate(sigma_s = sqrt(sum(df*sd^2)/sum(df)),
se = sigma_s / sqrt(nw),
upper = mean(xbar) + 3*se,
lower = mean(xbar) - 3*se
)
# Anwer for the q1a/q1b
stat_s%>%head(3)

#Q1C draw the average charge with label
labels <- stat_s %>%
  summarize(
    subgroup = max(subgroup),
    type = c("xbbar","upper","lower"),
    name  = c("mean", "+3 s", "-3 s"),
    value = c(mean(xbar), unique(upper), unique(lower)),
    value = round(value, 2),
    text  = paste(name, value, sep = " = ")
  )

# Visualize
avg_plot = stat_s %>%
  ggplot(mapping = aes(x = subgroup, y = xbar)) +
  geom_hline(mapping = aes(yintercept = mean(xbar)), color = "lightgrey", size = 3) +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
  geom_line(size = 1) +
  geom_point(size = 5) +
  # Plot labels
  geom_label(data = labels, mapping = aes(x = subgroup, y = value, label = text),  hjust = 1)  +
  labs(x = "Subgroup", y = "Average grams",
       subtitle = "Average Chart of products weight")

# Q2
library(ggpubr)
#create a small dataframe to calculate the range of groups
r_subgroup = products %>% 
  group_by(subgroup)%>%
  summarise(R= max(grams) - min(grams))
Rbar = mean(r_subgroup$R)

#reference the dn function 
dn = function(n, reps = 1e4){
  
  tibble(rep = 1:reps) %>%
    group_by(rep) %>%
    summarize(r = rnorm(n = n, mean = 0, sd = 1) %>% range() %>% diff() %>% abs()) %>%
    ungroup() %>%
    summarize(
      d2 = mean(r),
      d3 = sd(r),
      D3 = 1 - 3*(d3/d2), 
      D4 = 1 + 3*(d3/d2), 
      D3 = if_else(D3 < 0, true = 0, false = D3) ) %>%
    return()
}
# simulate the D3/D4
const = dn(5)
D3 = const$D3
D4 = const$D4
# set the upper and lower limit
upper = Rbar * D4
lower = max(0,D3*Rbar)
upper;lower
# Visualize it
labels_R <- tibble(
  subgroup = max(r_subgroup$subgroup),
  name  = c("R̄", "upper", "lower"),
  value = c(Rbar, upper, lower),
  text  = paste0(name, " = ", round(value, 3))
)

r_plot=ggplot(r_subgroup, aes(x = subgroup, y = R)) +
  geom_hline(yintercept = Rbar,  color = "grey40", linewidth = 1) +
  geom_hline(yintercept = upper, color = "red",   linetype = "dashed") +
  geom_hline(yintercept = lower, color = "red",   linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.6) +
  geom_label(data = labels_R, aes(x = subgroup, y = value, label = text), hjust = 1) +
  labs(title = "Range (R) Chart",
       subtitle = sprintf("R̄=%.3f, UCL=%.3f, LCL=%.3f (n=5)", Rbar, upper, lower),
       x = "Subgroup", y = "Range")
# combine the average chart and range chart using ggpubr
p_combined <- ggarrange(
  avg_plot, r_plot,
  ncol = 1, nrow = 2,       
  heights = c(1.1, 0.9),     
  labels = c("A", "B"),     
  align = "v"                
)

p_combined

#Q3 
sd_subgroup <- products %>% 
  group_by(subgroup) %>%
  summarise(s = sd(grams, na.rm = TRUE), .groups = "drop")

s_bar <- mean(sd_subgroup$s, na.rm = TRUE)
sd_subgroup%>%head(3)
s_bar

# 2)reference the function bx
bn <- function(n, reps = 1e4){
  tibble(rep = 1:reps) %>%
    group_by(rep) %>%
    summarize(s = sd(rnorm(n, mean = 0, sd = 1)), .groups = "drop") %>%
    summarize(b2 = mean(s), 
              b3 = sd(s),
              C4 = b2, 
              A3 = 3 / (b2 * sqrt(n)),
              B3 = 1 - 3 * b3/b2,
              B4 = 1 + 3 * b3/b2,
              B3 = if_else(B3 < 0, 0, B3),
              .groups = "drop")
}

b_const <- bn(5)
B3 <- b_const$B3
B4 <- b_const$B4

upper_s <- B4 * s_bar
lower_s <- max(0, B3 * s_bar) 

s_bar;upper_s;lower_s

# Label
labels_S <- tibble(
  subgroup = max(sd_subgroup$subgroup),
  name  = c("S̄", "UCL", "LCL"),
  value = c(s_bar, upper_s, lower_s),
  text  = paste0(name, " = ", round(value, 3))
)

# 4) Visualize
s_plot <- ggplot(sd_subgroup, aes(x = subgroup, y = s)) +  
  geom_hline(yintercept = s_bar,  color = "grey40", linewidth = 1) +
  geom_hline(yintercept = upper_s,  color = "red", linetype = "dashed") +
  geom_hline(yintercept = lower_s,  color = "red", linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_label(data = labels_S, aes(x = subgroup, y = value, label = text), hjust = 1) +
  labs(
    title = "Standard Deviation (S) Chart",
    subtitle = sprintf("S̄=%.3f, UCL=%.3f, LCL=%.3f (n=5)", s_bar, upper_s, lower_s),
    x = "Subgroup", y = "Standard Deviation (s)"
  )

s_plot

#Q5
LSL <- 2

# get mu and sigma_s and sigma_t
mu <- mean(products$grams, na.rm = TRUE)
sigma_t <- sd(products$grams, na.rm = TRUE)
sigma_s <- first(stat_s$sigma_s)

# import functiom
cpk <- function(mu, sigma_s, LSL = NULL, USL = NULL){
  if(!is.null(LSL)) a <- abs(mu - LSL) / (3 * sigma_s)
  if(!is.null(USL)) b <- abs(USL - mu) / (3 * sigma_s)
  if(!is.null(LSL) & !is.null(USL)) return(min(a,b))
  if(is.null(LSL)) return(b) else return(a)
}

ppk <- function(mu, sigma_t, LSL = NULL, USL = NULL){
  if(!is.null(LSL)) a <- abs(mu - LSL) / (3 * sigma_t)
  if(!is.null(USL)) b <- abs(USL - mu) / (3 * sigma_t)
  if(!is.null(LSL) & !is.null(USL)) return(min(a,b))
  if(is.null(LSL)) return(b) else return(a)
}
#Calculation
Cpk <- cpk(mu = mu, sigma_s = sigma_s, LSL = LSL)
Ppk <- ppk(mu = mu, sigma_t = sigma_t, LSL = LSL)

Cpk; Ppk

# Q6
LSL <- 2

# Bootstrapping sample
myboot <- tibble(rep = 1:1000) %>%
  group_by(rep) %>%
  reframe(products) %>%                 
  sample_n(size = n(), replace = TRUE)

# calculate statistic in each rep subgroup
mybootstat <- myboot %>%
  group_by(rep, subgroup) %>%
  summarise(
    xbar    = mean(grams, na.rm = TRUE),
    sigma_w = sd(grams, na.rm = TRUE),
    n_w     = n(),
    .groups = "drop_last"
  ) %>%
  # CMerge together then calculate Cpk
  mutate(df_w = n_w - 1) %>%
  group_by(rep) %>%
  summarise(
    limit_lower = LSL,
    limit_upper = NA_real_,
    xbbar       = mean(xbar, na.rm = TRUE),
    # short_term sigma
    sigma_s     = sqrt( sum(df_w * sigma_w^2, na.rm = TRUE) / sum(df_w, na.rm = TRUE) ),
    k_groups    = n(),                     # subgroup's number
    n_total     = sum(n_w, na.rm = TRUE),  # total number of subgroup 
    #  Cpk one side
    estimate    = cpk(mu = xbbar, sigma_s = sigma_s, LSL = limit_lower),
    .groups = "drop"
  )

# --- Q6(a) double_sideed 95% CI  & onside 95% CI LSL---
ci95   <- quantile(mybootstat$estimate, c(0.025, 0.975), na.rm = TRUE)
lb95   <- quantile(mybootstat$estimate, 0.05, na.rm = TRUE)

ci95
lb95

