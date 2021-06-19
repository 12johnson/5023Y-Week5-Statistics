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

### A major assumption of linear regression is that
### the residuals are normally distributed. 
### The standardised residuals for our model (y actual −y predicted )
### are stored in the $.std.resid column from the
### broom::augment() function

augment_gestation %>% 
  ggplot(aes(x = .std.resid)) +
  geom_histogram()+
  ggtitle("Histogram of the model residuals")

### code above creates a histogram showing the frequencies of std.resid

augment_gestation %>%
  ggplot(aes(sample = .std.resid)) +
  geom_qq()+
  stat_qq_line()+
  ggtitle("QQ plot")

### code above creates a qq plot to see if the residuals follow
### a normal distribution of errors

knitr::include_graphics("img/normal.png")
knitr::include_graphics("img/right skew.png")
knitr::include_graphics("img/left skew.png")
knitr::include_graphics("img/underdispersed.png")
knitr::include_graphics("img/overdispersed.png")

### first image is normally distributed data
### The normal distribution is symmetric,
### so it has no skew (the mean is equal to the median).
### On a Q-Q plot normally distributed data appears as roughly
### a straight line (although the ends of the Q-Q plot often
### start to deviate from the straight line).

### second image is right skewed data
### this example is an exponential distribution
### On a Q-Q plot right-skewed data appears curved 
### ABOVE THE LINE

### the third image is left skewed data
### this is an example of a negative exponential distribution.
### Left-skew is also known as negative skew.
### On a Q-Q plot left-skewed data appears curved 
### (the opposite of right-skewed data).
### UNDER THE LINE

### the fourth image is Under-dispersed data
### distribution that is under-dispersed relative to a normal 
### distribution (in this case it is the uniform distribution).
### Under-dispersed data has a reduced number of outliers
### (i.e. the distribution has thinner tails than a normal
### distribution). Under-dispersed data is also known as having a 
### platykurtic distribution and as having negative excess kurtosis.
### kurtosis - describes the frequency of distribution of
### score clusters in the tails or peak
### Platykurtic - Fewer values in the tails and
### fewer values close to the mean
### On a Q-Q plot under-dispersed data appears S shaped.

### the fifth image is of Over-dispersed data.
### over-dispersed relative to a normal distribution 
### (in this case it is a Laplace distribution). 
### Over-dispersed data has an increased number of outliers
### (i.e. the distribution has fatter tails than a normal distribution).
### Over-dispersed data is also known as having a 
### leptokurtic distribution and as having positive excess kurtosis.
### leptokurtic -More values in the distribution tails and 
### more values close to the mean 
### (i.e. sharply peaked with heavy tails)
### On a Q-Q plot over-dispersed data appears as a flipped S shape 
### (the opposite of under-dispersed data).

augment_gestation %>%
  ggplot(aes(x=.fitted, y= .std.resid)) +
  geom_point()+
  ggtitle("Standardised residuals against Fitted values from the model")

augment_gestation %>%
  ggplot(aes(x=.fitted, y= .resid)) +
  geom_point()+
  ggtitle("Residuals against Fitted values from the model")

### the pattern is identical but the scale is very different. 
### This is because your residuals are on the original scale
### of the dependent variable, whereas your standardised residuals
### have been fitted onto a distribution of standard deviation.
### Here we can see there is clearly both an increase in the
### variance of the residuals as the fitted values increase
### AND there is a clear trend in the residuals. Not looking good!
### Remember what we WANT to see is no trends here and an
### even distribution of the residuals - this would indicate 
### our model has explained most or all of the linear pattern, 
### and has an equal amount of error along the regression.

