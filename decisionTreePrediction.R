# Outline -----------------------------------------------------------------

# Here we build a function used to predict response variables using our trained decision tree model.



# Prediction Function -----------------------------------------------------

predictDecisionTree <- function(data,           # Our function takes the data to predict as a parameter
                                model){         # ... and the model used to make the predictions

  # Our model is in a nested list structure.
  # We can loop through the model and strip away layers of the model at each step until we reach a prediciton.
  # A $split.predictor element will only exist if the data gets split at the current node.
  
  # Let's begin...
  
  # First check if a $split.predictor element exists in the current node
  if(is.null(model$split.predictor)){
    
    # If it doesn't, assign the prediction output (and we're done)
    prediction <- model
    
  }else{
    
    # If we get here then a split must occur at the current node.
    # We pull the split information:
    split.predictor <- model$split.predictor
    split.value <- model$split.value
    
    # We use this information to determine which branch to go down:
    if(data[,split.predictor][[1]] <= split.value){
      model <- model$data.1 
    }else{
      model <- model$data.2
    }
    # Our model has now been stripped down to the relevant branch
    
    # Once we've reassigned y to be our chosen subdata set, we can recursively call our function:
    predictDecisionTree(data, model)
    
  }
  
  
}
# Finished!
