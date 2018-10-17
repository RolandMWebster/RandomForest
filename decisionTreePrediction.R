# Build of Decision Tree Model

# Current Status: A function that constructs a decision tree on a given dataset.

# Packages ----------------------------------------------------------------

library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)


# 1. Splitting Calc -------------------------------------------------------

subdata <- iris %>% 
  filter(Species %in% c("versicolor", "virginica")) %>%
  mutate(Species = as.character(Species)) %>%
  droplevels()




# Start Function ----------------------------------------------------------

trainDecisionTree <- function(data,
                              response,
                              predictors = names(data)[names(data) != response],
                              requiredCostReduction,
                              samplePredictorCount
){
  
  
  # Variables II ------------------------------------------------------------
  
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
  split.predictor <- as.character(split.table$name[which.max(split.table$cost.change)])
  split.value <- split.table$split.value[which.max(split.table$cost.change)]
  cost.value <- split.table$cost.value[which.max(split.table$cost.change)]
  cost.change <- split.table$cost.change[which.max(split.table$cost.change)]
  
  # Split Data if the cost change is great enough
  if(cost.change >= requiredCostReduction){
    data.1 <- data %>% filter_at(vars(split.predictor), any_vars(. <= split.value))
    data.2 <- data %>% filter_at(vars(split.predictor), any_vars(. > split.value))
    output <- list("data.1" = data.1,
                   "data.2" = data.2)
    
  # Recursively call the function
  output <- lapply(output,
                   trainDecisionTree,
                   response = response,
                   predictors = predictors,
                   requiredCostReduction = requiredCostReduction)
    
    
  # Bolt on the needed split information
  output <- c(output,
              "split.predictor" = split.predictor,
              "split.value" = split.value)
    
    
    
    # Determine our output when the cost reduction drops below our threshold
    # We need to return the predicted response value at this step:
  }else{
    
    # Tabulate frequency of response variables (the maximum freq will be our prediction)
    result.table <- as.data.frame(table(data[,response]))
    
    # Store our output as the response choice
    output <- list("prediction" = as.character(result.table$Var1[result.table$Freq == max(result.table$Freq)]),
                   "probability" = max(result.table$Freq) / sum(result.table$Freq)) # <--- NEED TO RENAME THIS TO THE CORRECT NAME
    
    
    # output <- data
    
    
  }
  
  
  
  
  
  
}


# TEST ======================================================================
model <- trainDecisionTree(data = subdata,
                           response = "Species",
                           requiredCostReduction = 0.01,
                           samplePredictorCount = 2)
str(model)
# ===========================================================================
