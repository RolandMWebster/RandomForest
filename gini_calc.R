# Packages ----------------------------------------------------------------

library(tidyr)
library(dplyr)
library(ggplot2)


# 1. Splitting Calc -------------------------------------------------------

subdata <- iris %>% 
  filter(Species %in% c("setosa", "virginica")) %>%
  dplyr::select(Species, Sepal.Length) %>%
  droplevels()

head(subdata)


# Variables ---------------------------------------------------------------

kObs <- nrow(subdata) # Number of observations in the data set
kResponseVals <- unique(subdata$Species) # Number of unique response values to classify

  
# GINI INDEX --------------------------------------------------------------

# Calculate cost before splitting
cost.before <- 1 - subdata %>% 
  group_by(Species) %>%
  summarise(gini = (n() / kObs)^2) %>%
  ungroup() %>%
  summarise(gini = sum(gini))


# Calculate cost for all unique splits in Sepal.Length
cost.vector <- c(rep(0,length(unique(subdata$Sepal.Length))))

for(i in 1:length(unique(subdata$Sepal.Length))){
  
  # Calculate cost for subset of data containing rows less than or equal to split value
  x1 <- subdata[subdata$Sepal.Length <= unique(subdata$Sepal.Length)[i],]

  cost1 <- 1 - (x1 %>% 
                   group_by(Species) %>%
                   summarise(gini = (n() / nrow(x1))^2) %>%
                   ungroup() %>%
                   summarise(gini = sum(gini)))[[1]]

  
  # Calculate cost for subset of data containing rows greater than split value
  x2 <- subdata[subdata$Sepal.Length > unique(subdata$Sepal.Length)[i],]
  
  cost2 <- 1 - (x2 %>% 
                    group_by(Species) %>%
                    summarise(gini = (n() / nrow(x2))^2) %>%
                    ungroup() %>%
                    summarise(gini = sum(gini)))[[1]]

  
  # Weight by probabilities
  cost <- ((nrow(x1)/kObs) * cost1) + ((nrow(x2)/kObs) * cost2)
  
  # Update cost vector with cost value for current split
  cost.vector[i] <- cost
  
}

cost.vector

min(cost.vector)


