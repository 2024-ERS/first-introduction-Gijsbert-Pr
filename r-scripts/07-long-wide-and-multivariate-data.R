########## Vegetation data: working with multivariate datasets
## reshaping from wide to long formats
## merging, filtering data data 
## make a faceted plot with ggplot
## make a heatmap plot with ggplot

# clear all data
remove(list=ls())

# load libraries
library(tidyverse)

# read the vegetation data from the google sheet
vdat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSJFU7gDlXuBM6hgWwcYJ-c_ofNdeqBe50_lDCEWO5Du3K7kPUDRh_esyKuHpoF_GbmBAoT4ZygrGWq/pub?gid=2036214007&single=true&output=csv")

# show the variables in the dataset 
names(vdat)

# show in which unique years  data were recorded
unique(vdat$year)

# reshape the data using tidyr piping from wide to long format, where species is a single variable instead of distributed over multiple columns, treat bare, litter and moss as if they are species (see the "data import cheatsheet")
# remove Salicornia.europaea and Salicornia.procumbens from the dataset
# as Salicornia.sp is their sum (the 2 species where not separated in earlier years)
# also remove the variables bare,litter,mosses 
vdat1<-vdat |>
  tidyr::pivot_longer(-c(year,TransectPoint_ID), # which cols are not species
                         names_to = "Species_ID", # name of species variable
                         values_to ="cover") |>  # name of abundance variable
  dplyr::filter(!Species_ID %in% c("bare","litter","mosses",
                                   "SalicEur","SalicPro")) # exclude these

#show the names of all the species in the dataset
names(vdat1)
unique(vdat1$Species_ID)

# find the most abundant species in the dataset
# add a variable to the dataset that is the rank number of the species 
# according to summed abundance of each species over
# the whole dataset (1=most abundant species)
vdat2<-vdat1 |>
  dplyr::group_by(Species_ID) |>
  dplyr::summarise(sumcov=sum(cover,na.rm=T)) |>
  dplyr::mutate(rank=rank(-sumcov)) |>
  arrange(rank)
# merge the files
vdat3<-left_join(vdat1,vdat2,by="Species_ID")


### plot the 5 most dominant species as a line diagram, cover (y) versus distance_m (x)with ggplot, separate plot for each year, each species with a different line color

# plot the change in cover along the distance  transect 
# and over the different years as a heatmap for the 10 most abundant species
# (using ggplot),separate heatmap plot per species
vdat3 |> dplyr::filter(rank<=10) |>
  ggplot(aes(x=factor(TransectPoint_ID),y=factor(-year),fill = cover)) +
    geom_tile() +
    scale_fill_gradient(low="yellow",high="red") +
    facet_wrap(~Species_ID,ncol=2)
   

# load the elevation data from 2017-2023, 
# select the variables year, distance and elevation_m, 
# and  add  the elevation_m variable to the vdat3 vegetation data of 2017-2020
elevdat<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv") %>%
  dplyr::select(year,TransectPoint_ID,elevation_m) %>%
  dplyr::filter(!is.na(TransectPoint_ID))
elevdat
# join with the vegetation data
vdat4<- vdat3 %>% dplyr::filter(year>=2017) # make new file with filtered vegetation data only year >= 2017
names(vdat4)
vdat5<- dplyr::left_join(vdat4,elevdat,by=c("year","TransectPoint_ID")) # join the result with the elevation data
vdat5

hist(vdat5$elevation_m) # 13 classes 

# plot the change in cover along the elevation  gradient 
#and over the different years as a heatmap for the 10 most abundant species
# (using ggplot),separate heatmap plot per species
vdat5 %>% filter(rank<=10) %>%
  ggplot(aes(x=cut(elevation_m,13),y=as.factor(-year),fill=cover)) +
  geom_tile() +
  scale_fill_gradient(low="yellow",high="red") +
  facet_wrap(~Species_ID,ncol=2)

cut(vdat5$elevation_m,13)
