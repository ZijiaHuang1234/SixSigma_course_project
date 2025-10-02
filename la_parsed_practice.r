library("dplyr")
library("ggplot2")
la_parished <- read.csv("C:/Workspace/SixSigma/course_github_repository/sysen/workshops/la_parishes.csv")
la_parished%>%glimpse()
hist(la_parished)

