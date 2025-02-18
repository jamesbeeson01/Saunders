#library(devtools)
#install_github("jamesbeeson01/Saunders")
library(tidyverse)
library(Saunders)

lm <- lm(log(circumference) ~ age, Orange)

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  geom_confidence(lm, exp)

lm <- lm(sqrt(circumference) ~ age, Orange)

ggplot(Orange, aes(x=age, y=circumference)) + 
  geom_point(color="orangered") +
  geom_confidence(lm, \(x) x^2)
