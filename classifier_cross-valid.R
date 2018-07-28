# Import packages
library(rpart)


#### Classification ####

# Train and test model using k-fold cross validation
set.seed(50)
data.rand <- data[sample(1:nrow(data)), ]  # Randomize data set
k <- 10  # Number of folds
fold <- round(nrow(data.rand) / k, 0)  # Size of fold
idx.head <- 0
idx.tail <- fold
dt.tp <- 0
dt.tn <- 0
dt.fp <- 0
dt.fn <- 0

# Set fold in iteration i as test set and remaining folds as training set
for (i in 1:k) {
  idx.head <- idx.head + 1
  idx.tail <- fold * i
  idx <- idx.head:idx.tail
  
  # Split data for training and testing
  data.test <- na.omit(data.rand[idx,])
  data.train <- data.rand[-idx,]
  
  # Build model
  dt.model <- rpart(V16 ~ ., data = data.train, method="class", parms = list(split="information"))
    
  # Test model
  dt.pred <- predict(dt.model, data.test[, names(data.test) != "V16"], type="class")

  
  #### Evaluation ####
  
  # Create data frame of actual and predicted classes
  dt.pred.df <- data.test
  dt.pred.df <- cbind(dt.pred.df, predict=dt.pred)
  names(dt.pred.df)[names(dt.pred.df) == "V16"] <- "actual"
  
  # Create confusion matrix
  dt.cm <- table(dt.pred.df$actual, dt.pred.df$predict, dnn=c("Actual","Predicted"))
  
  # Count correct classifications
  dt.tp <- dt.tp + dt.cm[2,2]  # True positive
  dt.tn <- dt.tn + dt.cm[1,1]  # True negative
  
  # Count incorrect classifications
  dt.fp <- dt.fp + dt.cm[1,2]  # False positive
  dt.fn <- dt.fn + dt.cm[2,1]  # False negative
  
  # Set current fold's tail as next fold's head 
  idx.head <- idx.tail
}

# Create confusion matrix of all iterations
matrix(c(dt.tp, dt.fp, dt.fn, dt.tn), ncol = 2, 
       dimnames = list(Actual = c("+", "-"), Predicted = c("+", "-")))

# Calculate accuracy
(dt.accu <- (dt.tp + dt.tn) / nrow(data))


cat(dt.tp, dt.tn, dt.fp, dt.fn, dt.accu, sep='\t')