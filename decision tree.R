
# Outline -----------------------------------------------------------------

# This code creates our function to split the data.
# It performs the following:
# 1. Use our Gini calc function to determine how to split the data.
# 2. Check if the cost reduction is great enough to warrant a split.
# 3. Return a list with either:
#   a. The original data set if the cost reduction is too low to warrant a split.
#   b. The two split datasets if the cost reduction is large enough to warrant a split.

#################### TO DO #########################

# Consider how to create a more dynamic framework.
# This will need to be looped until no more splits occur.

####################################################

# Packages ----------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)


# 1. Splitting Calc -------------------------------------------------------

subdata <- iris %>% 
  filter(Species %in% c("setosa", "virginica")) %>%
  mutate(Species = as.character(Species)) %>%
  droplevels()


# Variables I -------------------------------------------------------------

# This is input by the user when calling the decision tree function
input.predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
kResponse <- "Species"
kRequiredCostReduction <- 0.4




# Start Function ----------------------------------------------------------

splitData <- function(data,
                      response,
                      predictors,
                      requiredCostReduction
){
  

  
  
  
  # Variables II ------------------------------------------------------------
  
 # kObs <- nrow(data) # Number of observations in the data set - this is calculated within the splitData function
  kResponseVals <- unique(data[,response]) # All unique response values to classify
  
  
  
  # Prep Split Table --------------------------------------------------------
  
  split.table <- data.frame(name = predictors,
                            split.value = rep(0, length(predictors)),
                            cost.value = rep(0, length(predictors)),
                            cost.change = rep(0, length(predictors)))
  
  
  # Begin Loop Through Predictors -------------------------------------------
  
  for(i in split.table$name){
    
    # Standardize names of columns for use with calculateCost functions
    x <- data[, c(response, i)]
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

# Split Data if the cost change is great enough
if(cost.change >= requiredCostReduction){
data.1 <- subdata %>% filter_at(vars(split.predictor), any_vars(. <= split.value))
data.2 <- subdata %>% filter_at(vars(split.predictor), any_vars(. > split.value))
output <- list(data.1,
              data.2)


output <- lapply(output,
                 splitData,
                 response = response,
                 predictors = predictors,
                 requiredCostReduction = requiredCostReduction)



}else{
  output <- data
}








}

test <- splitData(data = subdata,
                  response = "Species",
                  predictors = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                  requiredCostReduction = 0.4)


