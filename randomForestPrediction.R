# Outline -----------------------------------------------------------------

# Here we build a function used to predict response variables using our trained random forest model.
# We utilize the predictDecisionTree() function that we built for predicting response variables from our decision tree model.
# No changes are made to the predictDecisionTree() function.



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




# Now To Predict Random Forest --------------------------------------------


predictRandomForest <- function(data,             # Our function takes the same parameters as predictDecisionTree()
                                model){

  # Initilialize an output data.frame
  
  # This should have length = number of observations to predict
  output <- data.frame("Prediction" = rep(0,nrow(data)),
                       "Freq" = rep(0,nrow(data)))
  
  
  
  # Loop through the observations in our data
  for(i in 1:nrow(data)){
    
    # Declare our current observation
    observation <- data[i,]
    
    # Initialize List of Predictions - we'll get a prediction for each tree in the forest
    predictions <- vector("list", length = length(model))
    
    # Loop through each tree in the random forest
    for(j in 1:length(model)){
      
      # Declare the current tree
      tree <- model[[j]]
      
      # Determine the prediction for tree j by calling our predictDecisionTree() function
      current.prediction <- predictDecisionTree(observation,tree)
      
      # Update our prediction list with the current prediction
      predictions[[j]] <- data.frame("Prediction" = current.prediction$prediction,
                                     "Probability" = current.prediction$probability)
      
      } # End our loop through our trees
    
    # Convert our list of predicitons to a data.frame
    predictions <- ldply(predictions) %>%
      group_by(Prediction) %>%
      summarise(Freq = n() ) %>%
      ungroup() %>%
      arrange(desc(Freq))
    
    output$Prediction[i] <- as.character(predictions$Prediction[[1]])
    output$Freq[i] <- predictions$Freq[1]
    
    
  } # End loop through observations
  
  
  # Print output
  output
  
}
# Finished
