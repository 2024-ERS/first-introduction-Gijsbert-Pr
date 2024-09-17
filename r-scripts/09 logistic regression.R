# Generalized linear modelling - logistic regression - microtransect
# clear all variables in memory - keep this at the start of each script
rm(list = ls())

# restore library
renv::restore()

# load required libraries
library(tidyverse)
library(patchwork)

# read the microtransect data, noting that the data are in wide format
dat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTtr76jvltCTUxKrwpWRg8CcaZlRPH7SsdGWFVzh7HJPCkgQCMSPIVPiwJxmQQCby_hjhankU-tRSfH/pub?gid=409330480&single=true&output=csv") 
dat
unique(dat$year)
names(dat)

# make a histogram of the elevations for year 2024
dat %>% 
  dplyr::filter(year==2024) %>%
  ggplot(aes(x=elevation_m)) +
     geom_histogram(binwidth = 0.01,alpha=0.5) +
     geom_density()

# convert the data to long format and filter for 2023, exclude Salicornia.sp
# sort by species and distance_m and 
dat1<-dat %>% 
  tidyr::pivot_longer(-c(year,Point_ID,x_coord,y_coord,elevation_m,claydepth_cm),
                      names_to="species",
                      values_to = "presence") %>%
  dplyr::arrange(year,species,Point_ID) %>%
  dplyr::filter(year==2024, species!='Salicornia.sp')
dat1


dat2<-dat1 %>% 
  dplyr::rename(clay_layer_cm=claydepth_cm) %>%
  dplyr::mutate(sand_layer_cm=100*elevation_m-clay_layer_cm) %>%
  dplyr::select(Point_ID,sand_layer_cm,clay_layer_cm) %>%
  tidyr::pivot_longer(-Point_ID,
                      names_to="soil_layer",
                      values_to = "layer_thickness_cm") 
dat2


# calculate the frequency (as a proportion) of occurrence of each species 
# in 2023, and sort according to frequency

####### select Limonium vulgare and analyze and plot its response to elevation


# calculate a logistic regression (also test quadratic term)
# this is a generalized linear model with logit link and bionomial distribtion


# save the predicted values of this model (probability of occurrence) as a variable


# add the predicted values of the model to the graph as a line


#### analyse the occurrence of Spergularia.marina in the same way
# Analyze and plot  Spergularia.marina  the same way
# calculate a logistic regression (also test quadratic term)
# save the predicted values of this model (probability of occurrence) as a variable
# add the predicted values of the model to the graph as a line




### analyse Salicornia.procumbens the same way



