# multivariate analysis of vegetation composition in relation to environmental factors
renv::restore()
# clear everything in memory (of R)
remove(list=ls())

library(vegan) # multivariate analysis of ecological community data 
library(psych) # usefull for panel plots of multivariate datasets
library(tidyverse)

# read the vegetation data
vegdat0<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSJFU7gDlXuBM6hgWwcYJ-c_ofNdeqBe50_lDCEWO5Du3K7kPUDRh_esyKuHpoF_GbmBAoT4ZygrGWq/pub?gid=2036214007&single=true&output=csv") |>
  filter(year==2023 & TransectPoint_ID<=1150) |>     # filter for only the only 2023 data and plots with vegetation
  dplyr::select(-c(year,bare,litter,mosses,SalicSpp)) |>  # select  only distance_m and the species names as variable to use
  tibble::column_to_rownames(var="TransectPoint_ID") # convert distance_m to the row names of the tibble

vegdat<-vegdat0 |> dplyr::select(which(colSums(vegdat0) != 0)) # remove species that did not occur in this year in any plot


# read the macrotransect elevation data
elevdat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv") %>%
  dplyr::filter(year==2023 & !is.na(TransectPoint_ID) & TransectPoint_ID<=1150) %>%
  dplyr::select(TransectPoint_ID,elevation_m)   # select  only distance_m and elevation 
elevdat

# read the macrotransect clay thickness from the soil profile dataset
claydat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQyEg6KzIt6SdtSKLKbbL3AtPbVffq-Du-3RY9Xq0T9TwPRFcgvKAYKQx89CKWhpTKczPG9hKVGUfTw/pub?gid=943188085&single=true&output=csv") |>
  dplyr::filter(Year==2023 & SoilType_ID %in% c("clay","clay_organic") & TransectPoint_ID<=1150) |>
  dplyr::select(TransectPoint_ID,corrected_depth) |>     
  group_by(TransectPoint_ID) |> 
  dplyr::summarize(clay_cm=mean(corrected_depth,na.rm=T)) #calculate average clay layer thickness  for each pole
claydat


##### read the flooding proportion (proportion of the time of  the growing season flooded)
# from 1 april - 30 aug
flooddat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTiJsz0xcUAhOjlTcHpT7NXhSysGMdzI6NfYm_kzpFtV99Eyea2up7dB-5a_P-jAKiwMBAXYURBTpRU/pub?gid=600890443&single=true&output=csv") |> 
  dplyr::filter(year==2023) |>
  dplyr::select(-year,-flood_ID)
flooddat

# read distance to gulley of each plot
gulleydist<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=2102201825&single=true&output=csv") |>
  dplyr::filter(Year==2023,TransectPoint_ID<=1150) |>
  dplyr::select(-Year,-DistanceToGullley_ID)
gulleydist
  
# also add redox
redox<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQyEg6KzIt6SdtSKLKbbL3AtPbVffq-Du-3RY9Xq0T9TwPRFcgvKAYKQx89CKWhpTKczPG9hKVGUfTw/pub?gid=1911552509&single=true&output=csv") |>
  dplyr::filter(Year==2023,TransectPoint_ID<=1150) %>%
  dplyr::group_by(TransectPoint_ID,ProbeDepth) %>%
  dplyr::summarize(redox_mV=mean(redox_raw2_mV,na.rm=T)) %>%
  tidyr::pivot_wider(id_cols=TransectPoint_ID,
                     names_from=ProbeDepth,
                     names_prefix = "redox",
                     values_from = redox_mV)
redox

##### merge elevation, clay thickness, flooding proportion and distance to gulley data in
# a sequential join operation in a pipe
# start from elevdat and use left_join, calling the tibble envdat
# set distance_m as the row names of the  tibble
# set the NAs for floodprop equal to zero
# . (dot) means " outcome of the pipe so far
envdat <-
  dplyr::left_join(elevdat,claydat,by="TransectPoint_ID") |>
  dplyr::left_join(flooddat,by="elevation_m") |>
  dplyr::left_join(gulleydist,by="TransectPoint_ID") |>
  dplyr::left_join(redox,by="TransectPoint_ID") |>
  dplyr::mutate(floodprob=ifelse(is.na(floodprob),0,floodprob),
                floodprob=round(floodprob,4),  # round the floodprob to 4 decimals
                clay_cm=ifelse(is.na(clay_cm),0,clay_cm)) |>
  tibble::column_to_rownames(var="TransectPoint_ID")

# for ordination with functions from the vegan library you need an separate environmental factors dataset
# and a species composition dataset with the same rownames, indicating  the same sites / samples
# the species data (community composition) need to be in wide format
vegdat
envdat

##### explore the correlations among the environmental factors in a panel pairs plot
psych::pairs.panels(envdat,smooth=F,ci=T,ellipses=F,stars=T,method="pearson") #make a bunch of scatterplots and look at the pearsons correlation coefficient (linear relation or not) for these plots
psych::pairs.panels(envdat,smooth=F,ci=T,ellipses=F,stars=T,method="spearman") #rank correlation coefficient (non-linear relation) (is a larger value of x related to a larger value of y) irrespective of the type of relation.
# note that the units are very different! 

##### Ordination: run a Principal Component Analysis (PCA) on the environmental data
# .scale=T means: use correlations instead of covariances
# use .scale=T for datasets where the variables are measured in different use

# first remove points with Na values (make rownames a column, filter and remove again)
envdat1<-envdat |> 
  dplyr::mutate(TransectPoint_ID=row.names(envdat)) |>
  dplyr::filter(!TransectPoint_ID %in% c("1000", "1050", "1100","1150")) |>
  dplyr::select(-TransectPoint_ID)http://127.0.0.1:32575/graphics/plot_zoom_png?width=1522&height=758

# do a principal component analysis (pca) (All NA's should be gone)
pca_env<-stats::prcomp(envdat, center=T, scale=T)
pca_env
summary(pca_env) #cumulative proportion: PC1 is 49% explaining + PC2 29% so together 78% of the variation in the data
#show the site scores for axis 1
pca_env$x

# the PCs are reduced dimensions of the dataset
# you reduce 6 variables to 2 dimensions
# make a biplot (variable scores plus sample score) the pca ordination
stats::biplot(pca_env) #ordination face plane
stats::biplot(pca_env,xlab="PC1 (49%)", ylab="PC2 (21%)")

##### ordination: calculate and plot a Non-metric Multidimensional Scaling (NMDS) ordination
# explore the distance (dissimilarity) in species composition between plots
d1<-vegan::vegdist(vegdat, method="euclidean") # calculate Euclidean dissimilarity matrix
d1 # show the dissimilarity matrix (100= completely different, 0= exactly the same)
d2<-vegan::vegdist(vegdat, method="bray") # calculate Bray-Curtis dissimilarity matrix)
d2

##### improve the NMDS ordination plot by only showing the dominant species
# non-metric multidimension scaling / indirect gradient analysis (only species composition)
nmds_veg<-metaMDS(vegdat,k=2, trace=F, trymax=1000, distance="bray") # trymax=1000 is the maximum number of iterations
nmds_veg
vegan::ordiplot(nmds_veg, type="t")
# show the ordination with the most abundance species with priority
SpecTotCov<-colSums(vegdat) # calculate the total cover of each species
SpecTotCov
vegan::ordiplot(nmds_veg, display="sites", cex=1, type="t") # plot the sites
vegan::orditorp(nmds_veg, display="species", priority= SpecTotCov, col="red", pcol="red", pch="+", cex=1.1) # add the species with the highest cover to the plot
#Priority starts with the most abundant ones and deletes overlapping ones
                
#Plots close to each other are more similar in species composition
#Species that are close to each other occus in the same plots

# and show the ordination with the most abundance species with priority


#### ordination: compare to a DCA -> decide what ordination we should do, linear or unimodal? 
# how long are the gradients? Should I use linear (PCA)or unimodal method (NMDS, DCA)
dca<-vegan::decorana(vegdat)
dca

# first axis is 8.1 standard deviations of species responses
# result: length of first ordination axis is >8 standard deviations
# only when <1.5 you can use a PCA or RDA
# plot the dca results as a biplot
vegan::ordiplot(dca, display="sites", cex=0.7, type="text", xlim=c(-5,5))
#DCA1 is the most important axis, DCA2 is the second most important axis
vegan::orditorp(dca, display="species", priority=SpecTotCov, col="red", pcol="red", pch="+", cex=0.8)
# add the species with the highest cover to the plot

##### fit the environmental factors to the dca ordination surface
names(envdat)
ef_dca<-vegan::envfit(dca~elevation_m+clay_cm+floodprob+DistGulley_m+redox5+redox10, data=envdat, na.rm=T)

#add the result to the ordination plot as vectors for each variable
plot(ef_dca, add=T)
##### add contour surfaces to the dca ordination for the relevant abiotic variables
vegan::ordisurf(dca,envdat$clay_cm, add=T, col="green")
vegan::ordisurf(dca,envdat$elevation_m, add=T, col="brown")
vegan::ordisurf(dca,vegdat$FestuRub, add=T, col="blue")
##### make the same plot but using a nmds
##### fit the environmental factors to the nmds ordination surface

##### fit the environmental factors to the dca ordination surface

#add the result to the ordination plot as vectors for each variable

##### add contour surfaces to the nmds ordination for the relevant abiotic variables


##### compare an unconstrainted (DCA) and constrained (CCA) ordination
# did you miss important environmental factors?
# show the results of the detrended correspondence analysis
dca
# the eigenvalues represent the variation explained by each axis
cca1<-vegan::cca(vegdat~elevation_m+DistGulley_m+floodprob+redox5+redox10+clay_cm, data=envdat)
summary(cca1)

# kick out variables that are least significant - simplify the model
#check what is significant
anova(cca1, by="axis")
anova(cca1, by="margin")

#redo with only important correlated variables
cca2<-vegan::cca(vegdat~floodprob+DistGulley_m, data=envdat)
summary(cca2)
anova(cca2, by="axis")
anova(cca2, by="margin")

# add the environmental factors to the cca ordination plot
vegan::ordiplot(cca2, display="sites", cex=1, type="text", xlab="CCA1 (21%)", ylab="CCA2 (14%)")
vegan::orditorp(cca2, display="species", priority=SpecTotCov, col="red", pcol="red", pch="+", cex=1.1)
vegan::ordisurf(cca2,envdat$floodprob, add=T, col="blue")  
vegan::ordisurf(cca2,envdat$DistGulley_m, add=T, col="green") 
# test if the variables and axes (margins) are significant

# You have measured the right things!
# for example - test this if you would have only measured clay thickness

# yes, clay thickness significantly affects vegetation composition

##### cluster analysis (classification) of  communities
# first calculate a dissimilarity matrix, using Bray-Curtis dissimilarity


 # show the dissimilarity matrix (1= completely different, 0= exactly the same)


# now cluster the sites based on similarity in species composition 
# using average linkage as the sorting algorithm


# back to  clustering based on species composition - show the dendrogram and cut in in 4 communities


##### add the clustering of plots to your cca ordination

#add the vegetation type to the environmental data

# test if DistGulley_m is different between the vegetation types

# what do you write: 
# the vegetation types were significantly different in distance to gulley (F3,18=21.36, P<0.001)
# * P<0.05, ** P<0.01, *** P<0.001

# means with the same letter are not significantly different