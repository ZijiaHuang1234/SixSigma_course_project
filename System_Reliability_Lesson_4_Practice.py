# example 1
import pandas as pd
import numpy as np
from sympy import *
import sys
import os
import matplotlib.pyplot as plt

# 添加functions_distributions的路径
sys.path.append(r'D:\codeWork\5300_repository\functions')
from functions_distributions import rnorm,density,tidy_density,approxfun

# 修复ggplot导入
from plotnine import ggplot, aes, geom_area, theme_classic, theme, labs

#create the lifespan distribution and build the PDF of lifetime distribution 
lifespan = rnorm(100,mean = 5,sd=2)
dlife =density(lifespan)
dlife = tidy_density(dlife)
dlife_fn = approxfun(dlife)
# And we can build the CDF here
plife_df = tidy_density(density(lifespan))
plife_df = plife_df.sort_values('x')
plife_df['y'] = plife_df['y'].cumsum() / plife_df['y'].sum()
plife = approxfun(plife_df)

time = np.arange(lifespan.min(),lifespan.max(),0.1)#time sequence
mycars = pd.DataFrame({
    'time':time,
    'prob':dlife_fn(time),
    "prob_cumulative":plife(time),
})
mycars['prob_survival'] = 1 - mycars['prob_cumulative']

#plot the three curves using ggplot
plot = (ggplot(mycars, aes(x='time')) +
  geom_area(aes(y='prob_cumulative'), fill='blue', alpha=0.5) +
  geom_area(aes(y='prob_survival'), fill='green', alpha=0.5) +
  geom_area(aes(y='prob'), fill='red', alpha=0.5) +
  theme_classic() + theme(legend_position='bottom') +
  labs(x='Lifespan of Car', y='Probability', subtitle='Example Life Distributions'))

print(plot)
plot.save("example_1_car_lifespan_distribution.png")





