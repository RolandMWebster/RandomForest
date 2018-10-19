# BUILD V.2.0 ADDING FACTOR COMPATIBILITY

calculateCostGINI <- function(x){
  
  kObs <- nrow(x)
  
  # Cost before is the same for factor and numerical variables
  cost.before <- 1 - x %>% 
    group_by(Response) %>%
    summarise(gini = (n() / nrow(x))^2) %>%
    ungroup() %>%
    summarise(gini = sum(gini))
  
  
  # Here we get a difference, we can just wrap this in an if() statement to start (kind of ugly though):
  
  # NUMERICAL VARIABLE ------------------------------------------------------
  
  if(is.numeric(x$Predictor) == TRUE){
  cost.vector <- c(rep(0,length(unique(x$Predictor))))
  
  for(i in 1:length(unique(x$Predictor))){
    
    # Calculate cost for subset of data containing rows less than or equal to split value
    x1 <- x[x$Predictor <= unique(x$Predictor)[i],]
    
    cost1 <- 1 - x1 %>% 
      group_by(Response) %>%
      summarise(gini = (n() / nrow(x1))^2) %>%
      ungroup() %>%
      summarise(gini = sum(gini))
    
    
    # Calculate cost for subset of data containing rows greater than split value
    x2 <- x[x$Predictor > unique(x$Predictor)[i],]
    
    cost2 <- 1 - x2 %>%
      group_by(Response) %>%
      summarise(gini = (n() / nrow(x2))^2) %>%
      ungroup() %>%
      summarise(gini = sum(gini))
    
    
    # Weight by probabilities
    cost <- ((nrow(x1)/kObs) * cost1) + ((nrow(x2)/kObs) * cost2)
    
    # Update cost vector with cost value for current split
    cost.vector[i] <- cost[[1]]
    
  }
  
  output <- data.frame("split.value" = unique(x$Predictor)[which.min(cost.vector)],
                       "cost.value" = min(cost.vector),
                       "cost.change" = cost.before[[1]] - min(cost.vector))
  
  }else{
  
  
  # NON-NUMERICAL VARIABLE --------------------------------------------------
  
    
    # Slightly different compared to a numerical predictor
    # We assign a list of all possible subsets (we take the floor of length of unique values / 2 so we don't repeat values)
    partitions <- unlist(lapply(1:floor(length(x$Predictor)/2),
                                combn,
                                x = x$Predictor,
                                simplify = FALSE),
                         recursive = FALSE)
    
    # As with the numerical precitors, initialize a cost vecotr
    cost.vector <- c(rep(0,length(partitions)))
    
    start.time <- Sys.time()
    for(i in 1:length(partitions)){
      
      # Take x1 as x with values in our subset
      x1 <- x[x$Predictor %in% partitions[i],]
      
      cost1 <- 1 - x1 %>% 
        group_by(Response) %>%
        summarise(gini = (n() / nrow(x1))^2) %>%
        ungroup() %>%
        summarise(gini = sum(gini))
      
      # Take x2 as x with the remaining values not in our subset
      x2 <- x[!(x$Predictor %in% partitions[i]),]
      
      cost2 <- 1 - x2 %>%
        group_by(Response) %>%
        summarise(gini = (n() / nrow(x2))^2) %>%
        ungroup() %>%
        summarise(gini = sum(gini))
      
      
      cost <- ((nrow(x1)/kObs) * cost1) + ((nrow(x2)/kObs) * cost2)
      
      cost.vector[i] <- cost[[1]]
      
      
    }
    
    print(Sys.time() - start.time)
    
    output <- data.frame("split.value" = unique(partitions)[[which.min(cost.vector)]],
                         "cost.value" = min(cost.vector),
                         "cost.change" = cost.before[[1]] - min(cost.vector))
    
  
  }
  
  
}  








# PLAYGROUND --------------------------------------------------------------
library(tidyr)
library(plyr)
library(dplyr)


data <- iris[,c("Species", "Sepal.Length")]


names(data) <- c("Response", "Predictor")

data <- data[sample(nrow(data),10),] %>%
  mutate(Predictor = as.character(Predictor))


output <- calculateCostGINI(data)
output

str(output)
