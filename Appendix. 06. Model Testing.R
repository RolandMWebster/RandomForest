# TESTING MODEL PERFORMANCE FOR DECISION TREE AND RANDOM FOREST

# TO DO:
# Correct predictDecisionTree() function




# Test Performance with some train/test data

train.proportion <- 0.7

set.seed(1111)
train.rows <- sample(nrow(iris), floor(nrow(iris)*train.proportion))

train.data <- iris[train.rows,]
test.data <- iris[-train.rows,]



# Fit Model on Train Data -------------------------------------------------

# Start with Decision Tree

# Log start time:
dtFitStart <- Sys.time()

dtModel <- trainDecisionTree(data = train.data,response = "Species",
                             requiredCostReduction = 0.005)

# Log end time:
dtFitEnd <- Sys.time()

# Get fit time:
(dfFitTime <- dtFitEnd - dtFitStart)


# Now Random Forest -------------------------------------------------------

# This will take a while to fit so we'll time it.

# Log start time:
rfFitStart <- Sys.time()

# Fit our model
rfModel <- trainRandomForest(data = train.data,response = "Species",
                             requiredCostReduction = 0.005,
                             treeCount = 100)

# Log end time:
rfFitEnd <- Sys.time()

# Get fit time:
(rfFitTime <- rfFitEnd - rfFitStart)




# Predict Response Variable on Test Data ----------------------------------

dtOutput <- predictDecisionTree(test.data, dtModel)
rfOutput <- predictRandomForest(test.data, rfModel)





# Create Results ----------------------------------------------------------

rfResults <- cbind(rfOutput, "Observed" = test.data$Species) %>%
  mutate(result = 1)
rfResults$result[rfResults$Prediction != rfResults$Observed] <- 0

rfResults <- rfResults %>%
  summarise(Result = sum(result) / n())

