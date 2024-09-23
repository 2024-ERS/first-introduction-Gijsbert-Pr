# linear mixed-effects models 

# clear everything in memory (of R)
remove(list=ls())

library(tidyverse)      # for piping, dplyr
library(lme4)          # for fitting mixed-effects models
library(lmerTest)     # for tests of significance of mixed-effects models
library(multcompView)
library(emmeans)

dat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ3XEZfbq9FoQZ2r1d4XBzCd7Dbn4Hh4IKphhJhKqQtoRQ0cNwLxjx_U3ZOlOfpUvm_ADHgzRf6wFks/pub?gid=0&single=true&output=csv") 


# plot the results for species richness, ignoring the block effects
ggplot(data=dat,aes(x=graztreat,y=specrich,fill=graztreat)) +
  geom_boxplot()

# explore the distribution of the species richness data within each treatment
dat %>% ggplot(aes(specrich)) +
  geom_histogram(binwidth = 3) +
  facet_grid(graztreat~.) +
  ylab("frequency") +
  xlab("plant species richness per 2m2")

# test with  linear model assuming the residuals are normally distributed,
# if plant species richness is different between the treatments 
# and inspect the Q-Q plot of the residuals, and do a Shapiro test for normality 
# do a one-way anova ( 1 predictor with >2 categories)


# use tukey test to see which groups are different


# explore the q-q plot of the residuals of the model to check normality


# plot the histogram of the residuals with normal curve


# run the standard linear model as a generalized linear model


# test with generalized linear model assuming now a poisson distribution of residuals 
# if plant species richness is different between the treatments 


# plot cover mean species richness



# test with standard linear model (assuming normal error  distribution) 
# if plant species richness is different between the treatments 
# and account for block effects in the design, assuming it is a fixed effect
# first only a model with only block , "reference model"


# test with  linear mixed model (assuming normal error  distribution) 
# if plant species richness is different between the treatments 
# and account for block effects in the design, assuming it is a random effect
# first fit a model with only block


# fixed slopes model, assuming the treatment effect is the same for every block


# random slopes model, assume the treatment effect is different per block

# test with generalized linear mixed model (poisson distribution) 
# if plant species richness is different between the treatments 
# and account for block effects in the design, assuming it is a random effect
# (the correct assumption)

# first fit a model with only block


# add treatment to the model, assuming fixed slopes (i.e. treatment effect is the same for every block)

# add treatment to the model, now  assuming random slopes and intercepts (i.e. treatment effect is the different for every block)


# Test if the model with grazing treatment is a better model, by comparing it to the model with block 
# only, calculation the AIC of each model and testing the difference with a Chi-square test

# -> final conclusion? what should I do? Go lmer

