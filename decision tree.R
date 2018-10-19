# Outline -----------------------------------------------------------------

# This code creates the function used to train the decision tree model
# We recursively call our function to create a nested list (which is our decision tree).

# Packages ----------------------------------------------------------------

# These are required for the function:
library(tidyr)
library(plyr)
library(dplyr)



# Start Function ----------------------------------------------------------

trainDecisionTree <- function(data,
                              response,
                              predictors = names(data)[names(data) != response],   # Default to all variables except the response variable
                              requiredCostReduction = 0.2)                         # Defualt to 0.2
){

  
  # Prep Split Table --------------------------------------------------------
  
  # We initialize our split.table, used to store information used by the model to determine splitting of data
  split.table <- data.frame(name = predictors,
                            split.value = rep(0, length(predictors)),
                            cost.value = rep(0, length(predictors)),
                            cost.change = rep(0, length(predictors)))
  
  
  
  # Begin Loop Through Predictors -------------------------------------------
  
  for(i in split.table$name){
    
    # Standardize names of columns for use with calculateCost function (currently only GINI)
    x <- data[, c(response, i)]
    names(x) <- c("Response", "Predictor")
    
    # Calculate costs used to determine split variable
    results <- calculateCostGINI(x)
    
    # Update split table.
    # For predictor i, we update with information from the split that results in the maximum reduction in cost
    split.table$split.value[split.table$name == i] <- results$split.value  
    split.table$cost.value[split.table$name == i] <- results$cost.value # <- currently not used but might be useful
    split.table$cost.change[split.table$name == i] <- results$cost.change  
    
  }
  # split.table now has all the information we need to determine what to do next
  
  
  # Determine Split ---------------------------------------------------------
  
  # We determine which variable we will split on IF a split should occur (this is the variable that gives us the largest reduction in cost)
  # We store this information:
  split.predictor <- as.character(split.table$name[which.max(split.table$cost.change)])
  split.value <- split.table$split.value[which.max(split.table$cost.change)]
  cost.value <- split.table$cost.value[which.max(split.table$cost.change)] # <- currently not used but might be useful
  cost.change <- split.table$cost.change[which.max(split.table$cost.change)]
  
  # We require the cost reduction to be greater than or equal to our requiredCostReduction parameter, we split only if this is the case:
  # Split Data if the cost change is great enough
  if(cost.change >= requiredCostReduction){
    
    # We will split our data into a list of two data.frames, this will allow us to use lapply
    data.1 <- data %>% filter_at(vars(split.predictor), any_vars(. <= split.value)) 
    data.2 <- data %>% filter_at(vars(split.predictor), any_vars(. > split.value))
    output <- list("data.1" = data.1,
                   "data.2" = data.2)
    
    # Recursively call our trainDecisionTree() function
    output <- lapply(output,
                     trainDecisionTree,
                     response = response,
                     predictors = predictors,
                     requiredCostReduction = requiredCostReduction)
    
    
    # Bolt on some needed split information
    output <- c(output,
                "split.predictor" = split.predictor,
                "split.value" = split.value)
    
    
    
  }else{
    # If the maximum cost reduction is below our chosen threshold, we do not split.
    # We need to determine our predicted response value:
    
    # Tabulate frequency of response variables (the maximum freq will be our prediction)
    result.table <- as.data.frame(table(data[,response]))
    
    # Store our output as the response choice
    output <- list("prediction" = as.character(result.table$Var1[result.table$Freq == max(result.table$Freq)]),
                   "probability" = max(result.table$Freq) / sum(result.table$Freq))
  }
}
# Finished!
