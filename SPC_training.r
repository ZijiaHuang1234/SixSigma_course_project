library(tidyverse)
library(viridis)
library(ggpubr)
library(moments)
#import data
water = read_csv("D:/codeWork/5300_repository/workshops/onsen.csv")
water %>% glimpse()

#15.1.1themeset

theme_set(
theme_classic(base_size = 14)+
  theme(
legend.position = "bottom",
plot.title = element_text(hjust = 0.5),
plot.subtitle = element_text(hjust = 0.5),
plot.caption = element_text(hjust = 0.5),
axis.ticks = element_blank(),
axis.line = element_blank(),
panel.border = element_rect(fill = NA, color ="grey"),
plot.margin = margin(r = 0)
)
)

# 15.1.2 Describitive statistics
describe = function(x){
  # Put our vector x in a tibble
  tibble(x) %>%
    # Calculate summary statistics
    summarize(
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      # We'll use the moments package for these two
      skew = skewness(x, na.rm = TRUE),
      kurtosis = kurtosis(x, na.rm = TRUE)) %>%
    # Let's add a caption, that compiles  all these statistics  
    mutate(
      # We'll paste() the following together
      caption = paste(
        # Listing the name of each stat, then reporting its value and rounding it, then separating with " | "
        "Process Mean: ", mean %>% round(2), " | ", 
        "SD: ", sd %>% round(2), " | ",
        "Skewness: ", skew %>% round(2), " | ",
        "Kurtosis: ", kurtosis %>% round(2), 
        # Then make sure no extra spaces separate each item
        sep = "")) %>%
    return()
}
# Run descriptives!
tab = water$temp %>% describe()
# Check it out!
tab
# 15.1.3 Visualize box_plot
g1 =  water %>% 
  ggplot(mapping = aes(x=time,y=temp, group =time)) + 
  geom_hline(mapping = aes(yintercept = mean(temp)),color = "lightgrey", size =3) +
  geom_jitter(height = 0,width = 0.25) +
  geom_boxplot() + 
  labs(x = "Time(Subgroup",y="Temperature(Cesius)",subtitle = "Process Overview",caption = tab$caption)
g1
# Make the histogram
g2 =  water %>% 
  ggplot(mapping = aes(x=temp)) +
  geom_histogram(bins = 15,color ="white",fill = "grey")+
  theme_void() +
  coord_flip()
g2
# combine g1 and g2
p1 = ggarrange(g1,g2,widths = c(5,1),align = "h")
p1

#Learning Check 1
overview_plot = function(x,y,xlab = "Subgroup",ylab="Metric"){
  #describe statisics of dependent variable
  tab = describe(y)
  #create chart 1 boxplot 
  chart_1 =  ggplot(mapping = aes(x=x,y=y, group=x)) + 
    geom_hline(mapping = aes(yintercept = mean(y)),color = "lightgrey", size =3) +
    geom_jitter(height = 0,width = 0.25) +
    geom_boxplot() + 
    labs(x = xlab,y=xlab,
          subtitle = "Process Overview",
          caption = tab$caption)
  # create histogram
  chart_2 =
    ggplot(mapping = aes(x = y)) +
    geom_histogram(bins = 15,color ="white",fill = "grey")+
    theme_void() +
    coord_flip()

  combine_chart = ggarrange(chart_1,chart_2,widths = c(5,1),align = "h") # generate the required charts for the result
  return(combine_chart)

}

overview_plot( x= water$time,y = water$ph)

#15.2.2 Subgroup(within-Group)
stat_s = water %>%
  group_by(time) %>%
  summarize(
    xbar = mean(temp),#within group average
    r = max(temp) - min(temp),
    sd =sd(temp),
    nw = n(),
    df = nw -1
  ) %>% mutate (
    sigma_s = sqrt(sum(df *sd^2)/sum(df)),
    sigma_s = sqrt(mean(sd^2)),
    se = sigma_s /sqrt(nw),
    upper = mean(xbar)+ 3*se,
    lower = mean(xbar- 3*se)
  )
stat_s%>% head(3)

#15.2.3 Total Statistic (Between Groups)
stat_t = stat_s %>%
  summarize(
    xbbar = mean(xbar),
    rbar = mean(r),
    sdbar = mean(sd),
    sigma_s = sqrt(mean(sd^2)),
    sigma_t = sd(water$temp)
  )
#15.2.4 Average andd standrd deviation charts
labels = stat_s%>%
   summarize(
    time = max(time),
    type = c("xbbar",  "upper", "lower"),
    name = c("mean", "+3 s", "-3 s"),
    value = c(mean(xbar), unique(upper), unique(lower)),
    value = round(value, 2),
    text = paste(name, value, sep = " = "))
stat_s %>%
  ggplot(mapping = aes(x = time, y = xbar)) +
  geom_hline(mapping = aes(yintercept = mean(xbar)), color = "lightgrey", size = 3) +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
  geom_line(size = 1) +
  geom_point(size = 5) +
  # Plot labels
  geom_label(data = labels, mapping = aes(x = time, y = value, label = text),  hjust = 1)  +
  labs(x = "Time (Subgroups)", y = "Average",
       subtitle = "Average and Standard Deviation Chart for temp")
# Learning Check 2
ph_s = water %>% group_by(time)%>%
  summarize(
    xbar = mean(ph),
    r = max(ph)-min(ph),
    sd =sd(ph),
    nw = n(),
    df = nw - 1
  ) %>%
  mutate(
    sigma_s = sqrt(mean(sd^2)), 
    se = sigma_s / sqrt(nw),
    upper = mean(xbar) + 3*se,
    lower = mean(xbar) - 3*se
  )
# labels it
labels = ph_s %>%
  summarize(
    time = max(time),
    type = c("xbbar",  "upper", "lower"),
    name = c("mean", "-3 s", "+3 s"),
    value = c(mean(xbar), unique(upper), unique(lower)),
    value = round(value, 2),
    text = paste(name, value, sep = " = "))
# visualize the chart
ph_s %>%
  ggplot(mapping = aes(x = time, y = xbar)) +
  geom_hline(mapping = aes(yintercept = mean(xbar)), color = "lightgrey", size = 3) +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
  geom_line(size = 1) +
  geom_point(size = 5) +
  # Plot labels
  geom_label(data = labels, mapping = aes(x = time, y = value, label = text),  hjust = 1)  +
  labs(x = "Time (Subgroups)", y = "Average pH",
       subtitle = "Average and Standard Deviation Chart for PH")

#15.3.1 moving range chart
# choosing first month as sample
indiv = water %>%
  filter(id %in% c(1, 21, 41, 61, 81, 101, 121, 141))
indiv$temp
indiv$temp%>%diff()%>%abs()
#15.3.2 Factors d2 and friends
mrsim = rnorm(n = 10000,mean = 0,sd =1)%>% diff() %>% abs()
mrsim %>% hist()

#15.3.2 factors d2 and friends