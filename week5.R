library(tidyverse)
library(modelr)
library(car)
library(qqplotr)
library(praise)
library(patchwork)
library(stargazer)
library(skimr)

mammals <- read_csv("data/mammals.csv")

### read in mammals data

skim(mammals)
str(mammals)
is.na(mammals)

### checking the data.

mammals %>% 
  select(!species) %>% 
  GGally::ggpairs()

### showing the relationships against species

p <- mammals %>% 
  ggplot(aes(x=gestation, y=body_wt))+
  geom_point()+
  ggtitle("Gestation (days) against \n Body Weight (kg)")

p

### plotting the data of gestation and body_wt


