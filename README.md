# RandomForest
Building a random forest model from scratch.

This repository contains the code for the following functions:

**fitRandomForest()** - Used to fit a random forest model to a set of training observations. The function uses a top-down, greedy approach for fitting each decision tree. 

**predictRandomForest()** - Used to predict the response value of a set of testing observations by using the fitted random forest model.



## The build of the random forest has the following sections:

**Appendix. 01. Gini Function.R** - Contains a function used within within the decision tree to determine the Gini Score (which is then used by the model to split the nodes of the decision trees).

**Appendix. 02. Decision Tree Model.R** - Contains a function used to fit a decsion tree model. This is an intermediary step, constructed as part of the random forest build.

**Appendix. 03. Decision Tree Prediction.R** - Contains a function used to classify response values using the decsion tree model. This is an intermediary step, constructed as part of the random forest build.

**Appendix. 04. Random Forest Model.R** - Contains a function used to fit a random forest model. This builds on the decision tree model code.

**Appendix. 05. Random Forest Prediction.R** - Contains a function used to classify response values using the random forestr model. This builds on the decision tree model code.

**Appendix. 06. Model Testing.R** - Contains code where both the decision tree model and random forest model are tested on the Iris data set.


## Limitations of the model:

* The model does not currently support non-numeric predictor variables.

Next steps for the model:

* Modify the model to accept non-numeric predictor variables,

* add more methods for node splitting (entropy etc.)
