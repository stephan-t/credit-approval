# Import packages
library(e1071)

# Import data
data.orig <- read.csv("data/credit_approval.csv", header = FALSE, na.strings = "?")
data <- data.orig


#### Data Transformation ####

# Normalize numeric attributes using min-max [0,1]
for (i in 1:ncol(data)) {
  if (is.numeric(data[, i])) {
    data[, i] <- (data[, i] - min(data[, i], na.rm = TRUE)) / 
      (max(data[, i], na.rm = TRUE) - min(data[, i], na.rm = TRUE))
  }
}


#### Data Cleaning ####

# Check for duplicates
anyDuplicated(data)

# Check for errors in categorical attributes
for (i in c(1, 4, 5, 6, 7, 9, 10, 12, 13)) {
  print(colnames(data)[i])
  print(levels(data[, i]))
}

# Check for missing values
anyNA(data)

# Fill in missing values for categorical attributes using naive Bayes
nb.model <- naiveBayes(V6 ~ ., data = data[, -16], na.action = "na.omit")
predict(nb.model, data[540, -c(6, 16)])

# Fill in missing values for numeric attributes using regression
l.model <- lm(V2 ~ ., data = data[, -16], na.action = "na.omit")
predict(l.model, data[93, -c(2, 16)])
