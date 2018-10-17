
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

# TRANSFORM INTO RANDOM FOREST BY CREAATING N NUMBER OF DECISION TREES
# ALLOW FOR RESPONSE VARIABLES WITH N > 2 LEVELS
# ALLOW FOR CHARACTER PREDICTORS
# ADD PARAMETER FOR MULTIPLE DIFFERENT COST FUNCTIONS

####################################################

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
                              predictors,
                              requiredCostReduction,
                              samplePredictorCount
){
  


# Loop Through Decision Trees ---------------------------------------------

# Sampling Predictors -----------------------------------------------------

# Filter our data down to our chosen predictor columns

(sample.predictors <- sample(predictors, samplePredictorCount))
    
data <- data[,c(response, sample.predictors)]  

  
  
  
  # Variables II ------------------------------------------------------------
  
 kResponseVals <- unique(data[,response]) # All unique response values to classify
  
  
  
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
    
    # Update split table to determine split variable
    split.table$split.value[split.table$name == i] <- results$split.value  
    split.table$cost.value[split.table$name == i] <- results$cost.value  
    split.table$cost.change[split.table$name == i] <- results$cost.change  
  
}
  
split.table

# OLD
# split.predictor <- as.character(split.table$name[which.min(split.table$cost.value)])
# split.value <- split.table$split.value[which.min(split.table$cost.value)]
# cost.value <- split.table$cost.value[which.min(split.table$cost.value)]
# cost.change <- split.table$cost.change[which.min(split.table$cost.value)]

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
                 predictors = sample.predictors,
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
                           predictors = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                           requiredCostReduction = 0.01,
                           samplePredictorCount = 2)
str(model)
# ===========================================================================



# Random Forest Model -----------------------------------------------------

trainRandomForest <- function(data,
                              response,
                              predictors,
                              requiredCostReduction,
                              samplePredictorCount,
                              treeCount){
  
  # Set progress bar
  pb <- txtProgressBar(min = 0, max = treeCount, style = 3)
  
  # Initialize list of n trees
  output <- vector("list", length = treeCount)
  
  # Name each element for readibility
  names(output) <- c(1:treeCount)
  
  # Use lapply with our trainDecisionTree() function to train n trees
  
  #### THIS ISN'T WORKING BECAUSE trainDecisionTree() IS USING output AS THE X PARAMETER
  #### MIGHT NEED A FOR LOOP FOR THIS
  # output <- lapply(output,
  #                  trainDecisionTree,
  #                  data = data,
  #                  response = response,
  #                  predictors = predictors,
  #                  requiredCostReduction = requiredCostReduction,
  #                  samplePredictorCount = samplePredictorCount)
  
  # FOR LOOP SOLUTION (EUGH)
  for(i in 1:treeCount){
    
    # Update progress bar
    setTxtProgressBar(pb, i)
    
    # Train n decision trees
    output[[i]] <- trainDecisionTree(data = data,
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
model <- trainRandomForest(data = subdata,
                           response = "Species",
                           predictors = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                           requiredCostReduction = 0.01,
                           samplePredictorCount = 2,
                           treeCount = 2)
str(model)
# ===========================================================================
