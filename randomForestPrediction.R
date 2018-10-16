
# Prediction Function -----------------------------------------------------

predictDecisionTree <- function(data, model){
  
  
  # Parameters:
  # x is the test data
  # y is the model

  # We're looking for an output of the form:
    # PREDICTION = P_1
    # PROBABILITY = P_2
  
  
  # We'll call the function recursively so we can keep y for all layers
    # If a split didn't occur, then we assign the output value:
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
  
  
  

# Determine Length of Model -----------------------------------------------

  
  
  
  
}



