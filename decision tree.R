# Packages ----------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)


# 1. Splitting Calc -------------------------------------------------------

subdata <- iris %>% 
  filter(Species %in% c("setosa", "virginica")) %>%
  droplevels()


# Variables I -------------------------------------------------------------

# This is input by the user when calling the decision tree function
input.predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
kResponse <- "Species"




# Variables II ------------------------------------------------------------

kObs <- nrow(subdata) # Number of observations in the data set - this is calculated by the model
kResponseVals <- unique(subdata$Species) # All unique response values to classify




# Prep Split Table --------------------------------------------------------

split.table <- data.frame(name = input.predictors,
                          split.value = rep(0, length(input.predictors)),
                          cost.value = rep(0, length(input.predictors)),
                          cost.change = rep(0, length(input.predictors)))


# Begin Loop Through Predictors -------------------------------------------

for(i in split.table$name){
  
  # Standardize names of columns for use with calculateCost functions
  x <- subdata[, c(kResponse, i)]
  names(x) <- c("Response", "Predictor")
  
  # Calculate costs used to determine split variable
  results <- calculateCostGINI(x)
  
  # Update split table to determine split variable
  split.table$split.value[split.table$name == i] <- results$split.value  
  split.table$cost.value[split.table$name == i] <- results$cost.value  
  split.table$cost.change[split.table$name == i] <- results$cost.change  
  
}
  
split.table

# Get required information for split
split.predictor <- as.character(split.table$name[which.min(split.table$cost.value)])
split.value <- split.table$split.value[which.min(split.table$cost.value)]
cost.value <- split.table$cost.value[which.min(split.table$cost.value)]
cost.change <- split.table$cost.change[which.min(split.table$cost.value)]

data.1 <- subdata %>% filter(split.predictor <= split.value)
data.2 <- subdata %>% filter(split.predictor > split.value)


