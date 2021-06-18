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

### species is a factor so we removed them from the ggpairs
### as an error would show up otherwise
### code shows the relationships against all other variables

p <- mammals %>% 
  ggplot(aes(x=gestation, y=body_wt))+
  geom_point()+
  ggtitle("Gestation (days) against \n Body Weight (kg)")

p

### plotting the data of gestation and body_wt

###Linear relationship: Maybe, though it is difficult to see
### because of the very narrow distribution of bodyweights
### across the mammals.
### Modeling concerns, definitely -
### it would be difficult to fit a good least squares line
### through this data.
### Clearly two very large outliers on both gestation and body weight
### at the high end of the scale.
### Also clearly a funnel shape as the values increase, 
### so major concerns there will be heteroscedasticity
###(unequal residual variance across the fit of our regression).


gestation_model <- lm(body_wt~gestation, data=mammals)
summary(gestation_model)

### creating a linear model
### Note that we haven’t checked all linear assumptions yet. 
### That’s because a lot of our assumptions for linear regression
### are based on model residuals 
### (e.g. normality & homoscedasticity of residuals), 
### which we can’t calculate until after we find the
### predicted values from the model (residual=y actual−y predicted).

### From this model we calculate that the, slope of the line is 4.12
### and the y-intercept is -374.97. getting this info out of
### the summary can be difficult but use code below

tidy_gestation <- broom::tidy(gestation_model, conf.int=T)
tidy_gestation

###this code forces the info from the summary into a tibble

tidy_gestation$estimate[1]
### this code is for the y-intercept

tidy_gestation$estimate[2]
### this is the code for the slope/gradient

glance_gestation <- broom::glance(gestation_model)
glance_gestation

### for other parts of the model, degrees of freedom, F-statistic,
### p-value, use broom::glance

augment_gestation <- broom::augment(gestation_model, interval="confidence")
augment_gestation

### to find the predicted values and the residuals for each species
### adult body weight for their gestation period,
### we can use broom::augment()
### to use this info, to manually evaluate some assumptions:
### Linearly related variables (CHECK - already looked & thought about this before starting).
### Normally distributed residuals.
### Homoscedasticity (constant residuals variance).
### Influential outliers.

### we are working with generalised linear models. 
### They can be tolerant to minor deviations from these assumptions,
### but if our assumptions are too severely violated
### then our regression may be a poor fit and
### our ability to determine significance,
### build confidence intervals and make predictions will be poor



