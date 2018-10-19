# Outline -----------------------------------------------------------------

# This code creates our function to split the data.
# It performs the following:
# 1. Use our Gini calc function to determine how to split the data.
# 2. Check if the cost reduction is great enough to warrant a split.
# 3. Return a list with either:

#   a. The original data set if the cost reduction is too low to warrant a split.
#   b. The two split datasets if the cost reduction is large enough to warrant a split.

#################### TO DO #########################

# Build splitting logic - DONE
# Recursively Call Splitting Process - DONE
# Add sampling of predictors - DONE
# Determine outputs for split / no split result - DONE
# TRANSFORM INTO RANDOM FOREST BY CREATING N NUMBER OF DECISION TREES - DONE

# ALLOW FOR RESPONSE VARIABLES WITH N > 2 LEVELS (I THINK THIS ALREADY WORKS!!!)
# ALLOW FOR CHARACTER PREDICTORS
# ADD PARAMETER FOR MULTIPLE DIFFERENT COST FUNCTIONS

####################################################

# Packages ----------------------------------------------------------------

library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)



# Load Data ---------------------------------------------------------------

# We load the iris data to train our model on
# We filter for only 2 response factors (we'll work on r > 2 solutions later)
subdata <- iris %>% 
  # filter(Species %in% c("versicolor", "virginica")) %>%
  mutate(Species = as.character(Species)) %>%
  droplevels()




# Start Function ----------------------------------------------------------

trainDecisionTree <- function(data,
                              response,
                              predictors = names(data)[names(data) != response],   # Default to all variables except the response variable
                              requiredCostReduction = 0.2,                         # Defualt to 0.2
                              samplePredictorCount = floor(length(predictors)^0.5) # Defualt to m = sqrt(p)
){
  
  # Sampling Predictors -----------------------------------------------------
  
  # Here is a change from our original decision tree, we're only alowed to use a subset of our predictors
  # A new subset is sampled at each split
  sample.predictors <- sample(predictors, samplePredictorCount)
  
  
  # Prep Split Table --------------------------------------------------------
  
  split.table <- data.frame(name = sample.predictors,
                            split.value = rep(0, length(sample.predictors)),
                            cost.value = rep(0, length(sample.predictors)),
                            cost.change = rep(0, length(sample.predictors)))
  
  
  # Begin Loop Through Predictors -------------------------------------------
  
  for(i in split.table$name){
    
    # Standardize names of columns for use with calculateCost functions
    x <- data[, c(response, i)]
    names(x) <- c("Response", "Predictor")
    
    # Calculate costs used to determine split variable
    results <- calculateCostGINI(x)
    
    # Update split table with information for the maximum cost reduction split for the current predictor i
    split.table$split.value[split.table$name == i] <- results$split.value  
    split.table$cost.value[split.table$name == i] <- results$cost.value  
    split.table$cost.change[split.table$name == i] <- results$cost.change  
    
  }
  # split.table now has all the information we need to determine what to do next
  

  # Determine Split ---------------------------------------------------------
  
  # We determine which variable we will split on IF a split should occur (this is the variable that gives us the largest reduction in cost)
  # We store information on this:
  split.predictor <- as.character(split.table$name[which.max(split.table$cost.change)])
  split.value <- split.table$split.value[which.max(split.table$cost.change)]
  cost.value <- split.table$cost.value[which.max(split.table$cost.change)]
  cost.change <- split.table$cost.change[which.max(split.table$cost.change)]
  
  # We require the cost reduction to be greater than or equal to our requiredCostReduction parameter, we split only if this is the case:
  # Split Data if the cost change is great enough
  if(cost.change >= requiredCostReduction){
    
    # We will split our data into a list of two data.frames, this will allow us to use lapply
    data.1 <- data %>% filter_at(vars(split.predictor), any_vars(. <= split.value))
    data.2 <- data %>% filter_at(vars(split.predictor), any_vars(. > split.value))
    output <- list("data.1" = data.1,
                   "data.2" = data.2)
    
    # Recursively call the function
    output <- lapply(output,
                     trainDecisionTree,
                     response = response,
                     predictors = sample.predictors,
                     requiredCostReduction = requiredCostReduction)
    
    
    # Bolt on the needed split information
    output <- c(output,
                "split.predictor" = split.predictor,
                "split.value" = split.value)
    
    
    
  }else{
  # Determine our output when the cost reduction is not large enough to split
  # We need to return the predicted response value at this step:
  
    # Tabulate frequency of response variables (the maximum freq will be our prediction)
    result.table <- as.data.frame(table(data[,response]))
    
    # Store our output as the response choice
    output <- list("prediction" = as.character(result.table$Var1[result.table$Freq == max(result.table$Freq)]),
                   "probability" = max(result.table$Freq) / sum(result.table$Freq)) # <--- NEED TO RENAME THIS TO THE CORRECT NAME
  }
}


# TEST ======================================================================
model <- trainDecisionTree(data = subdata,
                           response = "Species",
                           predictors = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                           requiredCostReduction = 0.01,
                           samplePredictorCount = 2)
str(model)
# ===========================================================================









# Now that we have appropriately modified our trainDecisionTree function to include the sampling of m predictors from p total 
# predictors, we can move onto the construction of our trainRandomForest function. The trainRandomForest function will call the 
# trainDecisionTree function in a loop to grow n trees.


# Random Forest Model -----------------------------------------------------

trainRandomForest <- function(data,
                              response,
                              predictors = names(data)[names(data) != response],    # Default to all variables except the response variable
                              requiredCostReduction = 0.2,                          # Defualt to 0.2
                              samplePredictorCount = floor(length(predictors)^0.5), # Defualt to m = sqrt(p)
                              treeCount,
                              bootstrapRatio = 0.8){
  
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
    
    # Train n decision trees
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




# TEST ======================================================================
model <- trainRandomForest(data = iris,
                           response = "Species",
                           predictors = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                           requiredCostReduction = 0.01,
                           samplePredictorCount = 2,
                           treeCount = 2)
str(model)
# ===========================================================================
