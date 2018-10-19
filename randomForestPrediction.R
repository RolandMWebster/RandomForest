
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




# Now To Predict Random Forest --------------------------------------------


predictRandomForest <- function(data, model){
  

  # Loop Through Each Observation to be Predicted ---------------------------

  output <- data.frame("Prediction" = rep(0,nrow(data)),
                       "Freq" = rep(0,nrow(data)))
  
  # Loop through the observations in our data
  for(i in 1:nrow(data)){
    
    observation <- data[i,]
    
    
    # Initialize List of Predictions ------------------------------------------
    
    predictions <- vector("list", length = length(model))
    
    # Loop through each tree in the random forest
    for(j in 1:length(model)){
      
      # Declare the current tree
      tree <- model[[j]]
      
      current.prediction <- predictDecisionTree(observation,tree)
      
      # Update the current prediction using the predictDecisionTree() function
      predictions[[j]] <- data.frame("Prediction" = current.prediction$prediction,
                                     "Probability" = current.prediction$probability)
      
      
      
      
    }
    
    # Convert our list of predicitons to a data.frame
    predictions <- ldply(predictions) %>%
      group_by(Prediction) %>%
      summarise(Freq = n() ) %>%
      ungroup() %>%
      arrange(desc(Freq))
    
    output$Prediction[i] <- as.character(predictions$Prediction[[1]])
    output$Freq[i] <- predictions$Freq[1]
    

  }


  
  output
  
}





# Let's take a random sample row and test our function --------------------

sample <- subdata[sample(nrow(iris))[1],]

output <- predictRandomForest(sample, model)

output




