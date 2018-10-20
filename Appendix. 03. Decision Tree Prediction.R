# Outline -----------------------------------------------------------------

# Here we build a function used to predict response variables using our decision tree.


# Prediction Function -----------------------------------------------------

# We start with a function that predicts a single observation.
# We can then wrap something round it.

predictObservation <- function(observation,    # Our function takes the observation to predict
                               model){         # ... and the model used to make the prediction
  
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
    if(observation[,split.predictor][[1]] <= split.value){
      model <- model$branch.1 
    }else{
      model <- model$branch.2
    }
    # Our model has now been stripped down to the relevant branch
    
    # Once we've reassigned y to be our chosen subdata set, we can recursively call our function:
    predictDecisionTree(observation, model)
    
  } # End our ifelse call
  
  
}




# Now To Predict Random Forest --------------------------------------------


predictDecisionTree <- function(data,             # Our function takes the full data set to predict
                                model){           # and similarly to the predictionObservation function, we provide the model as a parameter
  
  # Initilialize an output data.frame
  
  # This should have length = number of observations to predict
  output <- data.frame("Prediction" = rep(0,nrow(data)))
  
  
  # Loop through the observations in our data
  for(i in 1:nrow(data)){
    
    # Declare our current observation
    observation <- data[i,]
    
    # Determine the prediction for tree j by calling our predictDecisionTree() function
    output$Prediction[i] <- predictObservation(observation,model)
    
    
  } # End loop through observations
  
  # Return our output
  output
  
}
# Finished
