#### Mixed models using lme4 package

# clear all
remove(list=ls())
# restore libraries
renv::restore()

# load libraries
library(tidyverse)
library(lme4)  # for linear mixed models
library(lmerTest)  # and testing their significance

# Set seed for reproducibility
set.seed(123)

# Define the effect size for the treatment as a continuous factor
treatment_effect1 <- c(5, 10, 15, 20)
treatment_effect2<- c(5, 8, 12, 15)
treatment_effect3<- c(20, 25, 27, 30)


# Define the means for the three blocks
block_means <- c(5, 10,15)

# Define the number of observations per block
n <- 10

# Initialize an empty data frame to store the results
data <- data.frame()

# Loop over each block and each treatment level to generate the data
for (block in 1:3) {
  for (treatment in 1:4) {
    # Calculate the mean for this block-treatment combination, only for block 1 and 2
      if(block==1) {mean_value <- treatment * treatment_effect1[treatment] + block_means[block]}
      else if(block==2) {mean_value <- treatment * treatment_effect2[treatment] + block_means[block]}
      else  {mean_value <- treatment * treatment_effect3[treatment] + block_means[block]}    

    # Generate random values for each treatment level within the block
    values <- rnorm(n, mean = mean_value, sd = 5)
    
    # Create a temporary data frame for this block-treatment combination
    temp_data <- data.frame(
      Block = paste("Block", block, sep = ""),
      Treatment = treatment,  # Treating treatment as a continuous variable
      Value = values
    )
    
    # Combine with the main data frame
    data <- rbind(data, temp_data) |> as_tibble()
  }
}

# Display the first few rows of the data
head(data)


# show in a scatter plot



# find the best model describing the effects of treatment and block
# block is a random effect, treatment is a fixed effect



#using lme4, show a mixed model with fixed slopes (=effect of the treatment within each block) and random intercepts


#using lme4, show a mixed model with random slopes and random intercepts


# note that the effect of treatment is now also shown as a random effect 

# plot this model with the data as points using ggplot and predicted values

