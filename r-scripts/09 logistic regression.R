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
  tidyr::pivot_longer(-c(year,Point_ID,x_coord,y_coord,elevation_m,claydepth_cm,distance_rtk_m),
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
p1<-ggplot(data=dat2, aes(x=Point_ID, y=layer_thickness_cm, fill=soil_layer)) +
  geom_area() + 
  coord_cartesian(ylim=c(95,125)) #zoom in instead of deleting data
p1

# calculate the frequency (as a proportion) of occurrence of each species 
# in 2023, and sort according to frequency
dat3<-dat1 |>
  dplyr::filter(presence==1) |>
  dplyr::mutate(speciesnum=-as.numeric(factor(species))) #each species gets a unique number
p2<-ggplot(data=dat3, aes(x=Point_ID, y=speciesnum, col=species)) +
  geom_point(size=2)
p2
p2+p1+patchwork::plot_layout(ncol=1) #plot together

####### select Limonium vulgare and analyze and plot its response to elevation
#so we need a binomial distribution
specsel<-"Spergularia.marina"  
p3<-dat1 |>
  dplyr::filter(species==specsel) |>
  ggplot(aes(x=elevation_m, y=presence)) +
  geom_point(shape="|") + #change the shape to a vertical line
  ggtitle(specsel)
p3

# calculate a logistic regression (also test quadratic term)
# this is a generalized linear model with logit link and binomial distribution
m1<-dat1 |>
  dplyr::filter(species==specsel) |>
  glm(presence ~ elevation_m, family=binomial(link=logit), data=_) #logit link function (probability of occurance)

# save the predicted values of this model (probability of occurrence) as a variable
dat1<-dat1 |>
  dplyr::mutate(predicted=ifelse(species==specsel, predict(m1, type="response"),NA)) #If the species is limonium vulgare, predict the probability of occurance, otherwise NA
# type="response" gives the probability of occurance
p3 + geom_line(data=dat1|>
                 dplyr::filter(!is.na(predicted)),
               aes(y=predicted, col=species), linewidth=1.3) # add the predicted values of the model to the graph as a line

#test for quadratic effect
m2<-dat1 |>
  dplyr::filter(species==specsel) |>
  glm(presence ~ elevation_m+I(elevation_m^2), family=binomial(link=logit), data=_) 

anova(m2,m1, test="Chisq") #compare the two models (glm so Chisq))
dat1<-dat1 |>
  dplyr::mutate(predicted=ifelse(species==specsel, predict(m2, type="response"),NA)) 
p3 + geom_line(data=dat1|>
                 dplyr::filter(!is.na(predicted)),
               aes(y=predicted, col=species), linewidth=1.3) 
p3
# add the predicted values of the model to the graph as a line


#### analyse the occurrence of Spergularia.marina in the same way
# Analyze and plot  Spergularia.marina  the same way
# calculate a logistic regression (also test quadratic term)
# save the predicted values of this model (probability of occurrence) as a variable
# add the predicted values of the model to the graph as a line




### analyse Salicornia.procumbens the same way



