# RandomForest

### Overview

Building a random forest algorithm from scratch.

We build a fitDecisionTree() function that uses a top-down, greedy approach to fit a decision tree and uses the Gini Index to split nodes (and determine tree length). We then adapt this code to create a fitRandomForest() function. We add bootstrapping of n observations limit a selection of m predictors from p total predictors at each split. We use a loop to fit multiple decision trees to create our random forest model.


### Codes:

**Appendix. 01. Gini Function.R** - Contains a function used within within the decision tree to determine the Gini Score (which is then used by the model to split the nodes of the decision trees).

**Appendix. 02. Decision Tree Model.R** - Contains a function used to fit the decsion tree model. This is an intermediary step, constructed as part of the random forest build.

**Appendix. 03. Decision Tree Prediction.R** - Contains a function used to classify response values using the decsion tree model. This is an intermediary step, constructed as part of the random forest build.

**Appendix. 04. Random Forest Model.R** - Contains a function used to fit the random forest model. This builds on the decision tree model code.

**Appendix. 05. Random Forest Prediction.R** - Contains a function used to classify response values using the random forest model. This builds on the decision tree model code.

**Appendix. 06. Model Testing.R** - Contains code where both the decision tree model and random forest model are tested on the Iris data set.


### Limitations:

* The model does not currently support non-numeric predictor variables.

### Next Steps:

* Modify the model to accept non-numeric predictor variables,

* add more methods for node splitting (entropy etc.)
