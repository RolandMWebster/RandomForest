
# Prediction Function -----------------------------------------------------

predictRandomForest <- function(x, y){
  
  
  # Parameters:
  # x is the test data
  # y is the model

  # We're looking for an output of the form:
    # PREDICTION = P_1
    # PROBABILITY = P_2
  
  
  # REALLY NEED TO GET MY HEAD AROUND THE FORM OF THE MODEL
  
  
  # Let's start thinking about the logic:
  
  # For i in kNumberOfTrees <--- loop through the number of trees in the model
  
  # Let y be be the current tree.
  # We'll call the function recursively so we can keep y for all layers
  
  
  # If a split didn't occur, then we assign the output value:
  if(is.null(y$split.predictor)){
    
    output <- y
      
  }else{
    
    # So now we know a split occured, we need to determine which data set to go to by using our input value
    # Get our split.predictor and split.value
    split.predictor <- y$split.predictor
    split.value <- y$split.value
    
    # We determine our dataset to go to:
    if(x[,split.predictor][[1]] <= split.value){
      y <- y$data.1
    }else{
      y <- y$data.2
    }
    
    # Once we've reassigned y to be our chosen subdata set, we can recursively call our function
    predictRandomForest(x,y)
    
  }
  
  
}



# Let's take a random sample row and test our function --------------------

sample <- subdata[sample(nrow(subdata))[1],]

output_test <- predictRandomForest(sample, model)
output_test

sample
