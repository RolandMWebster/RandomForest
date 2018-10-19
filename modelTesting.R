# Test Performance with some train/test data

train.proportion <- 0.7

train.rows <- sample(nrow(iris), floor(nrow(iris)*train.proportion))

train.data <- iris[train.rows,]
test.data <- iris[-train.rows,]


# Train Model on Training Data --------------------------------------------

model <- trainRandomForest(data = train.data,
                           response = "Species",
                           requiredCostReduction = 0.005,
                           treeCount = 100)



# Predict Response Variable on Test Data ----------------------------------

output <- predictRandomForest(test.data, model)





# Create Results ----------------------------------------------------------

results <- cbind(output, "Observed" = test.data$Species) %>%
  mutate(result = 1)

results$result[results$Prediction != results$Observed] <- 0
