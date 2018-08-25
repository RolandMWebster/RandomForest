# Packages ----------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)


# 1. Splitting Calc -------------------------------------------------------

subdata <- iris %>% 
  filter(Species %in% c("setosa", "virginica")) %>%
  droplevels()

head(subdata)

kResponse <- "Species"

# Variables I -------------------------------------------------------------

kObs <- nrow(subdata) # Number of observations in the data set

input.predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")


# Variables ---------------------------------------------------------------

kResponseVals <- unique(subdata$Species) # All unique response values to classify


split.table <- data.frame(name = input.predictors,
                          split.value = rep(0, length(input.predictors)),
                          cost.value = rep(0, length(input.predictors)))


# Begin Loop Through Predictors -------------------------------------------

for(i in kPredictors$name){
  
  # Standardize names of columns for use with calculateCost functions
  x <- subdata[, c(kResponse, i)]
  names(x) <- c("Response", "Predictor")
  
  # Calculate costs used to determine split variable
  results <- calculateCostGINI(x)
  
  # Update split table to determine split variable
  split.table$split.value[split.table$name == i] <- results$split.value  
  split.table$cost.value[split.table$name == i] <- results$cost.value  

}
  
split.table

split.predictor <- split.table$name[which.min(split.table$cost.value)][1]
split.value <- split.table[which.min(split.table$cost.value), split.table$name]


data.1 <- subdata[subdata$split.table$name[which.min(split.table$cost.value)] <=
                    split.table$split.value[which.min(split.table$cost.value)],]

data.2 <- subdata[subdata$split.table$name[which.min(split.table$cost.value)] >
                    split.table$split.value[which.min(split.table$cost.value)],]
