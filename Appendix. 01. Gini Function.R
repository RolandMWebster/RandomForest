calculateCostGINI <- function(x){
  
  kObs <- nrow(x)
  
  cost.before <- 1 - x %>% 
    group_by(Response) %>%
    summarise(gini = (n() / nrow(x))^2) %>%
    ungroup() %>%
    summarise(gini = sum(gini))
  
  cost.vector <- c(rep(0,length(unique(x$Predictor))))
  
  for(i in 1:length(unique(x$Predictor))){
    
    # Calculate cost for subset of data containing rows less than or equal to split value
    x1 <- x[x$Predictor <= unique(x$Predictor)[i],]
    
    cost1 <- 1 - x1 %>% 
      filter(Predictor <= unique(x$Predictor)[i]) %>%
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
  
}  

