test <- c("black", "blue", "red", "yellow", "green")

res <- unlist(lapply(1:2,
                     combn, 
                     x = test, 
                     simplify = FALSE), 
              recursive = FALSE)
res
# This gives us all splits






# Try taking only partitions of length <= 2


# Maybe we just create the markdown document and then do this?
