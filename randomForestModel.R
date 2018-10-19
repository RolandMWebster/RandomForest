# Outline -----------------------------------------------------------------

# This code creates the function used to train the random forest model
# We start by utilizing the code from the trainDecisionTree() function.
# We make an edit to the tranDecisionTree() function so that it samples m predictors from p before each split.

############################# TO-DO ##################################

# Update "probability" output to give meaningful output

######################################################################

########################### LIMITATIONS ##############################

# Model does not currently accept non-numerical predictors
# Model does not limit splitting via number of datapoints in node

######################################################################



# Packages ----------------------------------------------------------------

# These are required for the function:
library(tidyr)
library(plyr)
library(dplyr)



# Start Function ----------------------------------------------------------

# As mentioned, start by creating a modified version of the trainDecisionTree() function.
# We add the samplePredictorCount parameter (this can be set to length(predictors) for a decision tree)
trainDecisionTree <- function(data,
                              response,
                              predictors = names(data)[names(data) != response],   # Default to all variables except the response variable
                              requiredCostReduction = 0.2,                         # Defualt to 0.2
                              samplePredictorCount = floor(length(predictors)^0.5) # Defualt to m = sqrt(p)
){
  
  # Sampling Predictors -----------------------------------------------------
  
  # Here is a change from our original decision tree; we're only using a subset of our predictors at each split
  sample.predictors <- sample(predictors, samplePredictorCount)
  
  
  # Prep Split Table --------------------------------------------------------
  
  # We initialize our split.table, used to store information used by the model to determine splitting of data
  split.table <- data.frame(name = sample.predictors,
                            split.value = rep(0, length(sample.predictors)),
                            cost.value = rep(0, length(sample.predictors)),
                            cost.change = rep(0, length(sample.predictors)))
  
  
  
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
                     predictors = sample.predictors,
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


# Now that we have appropriately modified our trainDecisionTree() function to include the sampling of m predictors from p total 
# predictors, we can move onto the construction of our trainRandomForest() function. The trainRandomForest() function will call the 
# trainDecisionTree() function in a loop to grow n trees.


# Random Forest Model -----------------------------------------------------

trainRandomForest <- function(data,
                              response,
                              predictors = names(data)[names(data) != response],    # Default to all variables except the response variable
                              requiredCostReduction = 0.2,                          # Defualt to 0.2
                              samplePredictorCount = floor(length(predictors)^0.5), # Defualt to m = sqrt(p)
                              treeCount,                                            # NEW to determine number of decision trees to grow
                              bootstrapRatio = 0.8){                                # NEW to determine bootstrap sample size
  
  # A bit of housekeeping ---------------------------------------------------
  
  # Set progress bar
  pb <- txtProgressBar(min = 0, max = treeCount, style = 3)
  
  # Initialize list of n trees
  output <- vector("list", length = treeCount)
  
  # Name each element for readibility
  names(output) <- c(1:treeCount)
  
  
  # Grow random forest ------------------------------------------------------
  
  # Loop through our treeCount variable to grow n trees
  for(i in 1:treeCount){
    
    # Update progress bar
    setTxtProgressBar(pb, i)
    
    # Perform our bootstrap selection of observations
    sample.data <- data[sample(nrow(data),floor(nrow(data)*bootstrapRatio), replace = TRUE),]
    
    # Train n decision trees on our bootstrapped data
    output[[i]] <- trainDecisionTree(data = sample.data,
                                     response = response,
                                     predictors = predictors,
                                     requiredCostReduction = requiredCostReduction,
                                     samplePredictorCount = samplePredictorCount)
  }
  
  # Close progress bar
  close(pb)
  
  # Return list of decision trees
  output
  
}
# Finished!
