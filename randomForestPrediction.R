
# Prediction Function -----------------------------------------------------

predictDecisionTree <- function(data, model){
  

  # We're looking for an output of the form:
  # PREDICTION = X
  # PROBABILITY = Y
  
  # We'll call the function recursively so we can keep y for all layers.
  
  # If a split didn't occur in the model, then we assign the output value:
  if(is.null(model$split.predictor)){
    
    prediction <- model
    
  }else{
    
    # If the cost reduction is large enough that we split the data then we need to determine which data set to go to by using our input value
    # Get our split.predictor and split.value
    split.predictor <- model$split.predictor
    split.value <- model$split.value
    
    # We determine our dataset to go to:
    if(data[,split.predictor][[1]] <= split.value){
      model <- model$data.1
    }else{
      model <- model$data.2
    }
    
    # Once we've reassigned y to be our chosen subdata set, we can recursively call our function
    predictDecisionTree(data, model)
    
  }
  
  
}




# Let's take a random sample row and test our function --------------------

sample <- subdata[sample(nrow(subdata))[1],]

output_test <- predictDecisionTree(sample, model)
output_test

sample






# Now To Predict Random Forest --------------------------------------------


predictRandomForest <- function(data, model){
  
  
  
  # Initialize List of Predictions ------------------------------------------
  
  predictions <- vector("list", length = length(model))
  
  # Loop through each tree in the random forest
  for(i in 1:length(model)){
    
    # Declare the current tree
    tree <- model[[i]]
    
    current.prediction <- predictDecisionTree(data,tree)
    
    # Update the current prediction using the predictDecisionTree() function
    predictions[[i]] <- data.frame("Prediction" = current.prediction$prediction,
                                   "Probability" = current.prediction$probability)
    
 
    
    
  }
  
  predictions <- ldply(predictions)
  
}



output <- predictRandomForest(sample, model)




