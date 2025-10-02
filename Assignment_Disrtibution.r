install.packages(c("ggplot2","dplyr","MASS"))
library("dplyr")
library("ggplot2")
library("MASS")
#data input
corgri = c(5, 1, 10, 3, 4, 3, 6, 4, 5, 2)
#calculate the required stats information
weibull_corgri_stats <- fitdistr(corgri,"weibull")
corgri_w_shape <- weibull_corgri_stats$estimate[1]
corgri_w_scale <- weibull_corgri_stats$estimate[2]
#simulate the weibull distribution
weibull_corgri <- rweibull(n = 1000,shape = corgri_w_shape,scale = corgri_w_scale)
#visualize
weibull_corgri%>%hist()
