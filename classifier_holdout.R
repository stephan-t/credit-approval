# Import packages
library(rpart)
library(rpart.plot)


#### Classification ####

# Split data for training and testing using holdout method
set.seed(50)
idx <- sample(1:nrow(data), floor(nrow(data) * (2/3)))
data.train <- data[idx,]  # 70% of data set
data.test <- data[-idx,]  # 30% of data set

# Build model
dt.model <- rpart(V16 ~ ., data = data.train, method="class", parms = list(split="information"))

# Plot model
rpart.plot(dt.model)

# Test model
dt.pred <- predict(dt.model, data.test[, names(data.test) != "V16"], type="class")


#### Evaluation ####

# Create data frame of actual and predicted classes
dt.pred.df <- data.test
dt.pred.df <- cbind(dt.pred.df, predict=dt.pred)
names(dt.pred.df)[names(dt.pred.df) == "V16"] <- "actual"

# Create confusion matrix
(dt.cm <- table(dt.pred.df$actual, dt.pred.df$predict, dnn=c("Actual","Predicted")))

# Calculate accuracy
(dt.accu <- sum(diag(dt.cm))/sum(dt.cm))

cat(dt.cm[2,2], dt.cm[1,1], dt.cm[1,2], dt.cm[2,1], dt.accu, sep='\t')