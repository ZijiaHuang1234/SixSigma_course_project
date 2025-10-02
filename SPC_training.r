library(tidyverse)
library(viridis)
library(ggpubr)
library(moments)
#import data
water = read_csv("sysen/workshops/onsen.csv")
water %>% glimpse()

#themeset

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

# Describitive statistics
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
# Visualize box_plot
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
    xbar = mean(temp),
    r = max(temp) - min(temp),
    sd =sd(temp),
    nw = n(),
    df = new -1
  ) %>%

  mutate (
    sigma_s = sqrt(sum(df *sd^2)/sum(df)),
    sigma_s = sqrt(mean(dd^2))
  )