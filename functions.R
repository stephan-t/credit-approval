# Import packages
library(e1071)  # naive Bayes


#### Data Cleaning ####

# Naive Bayes model evaluation
nb.model.eval <- function(y, x, data) {
  # Create train/test data of complete cases
  data <- na.omit(data)
  set.seed(10)
  idx <- sample(1:nrow(data), floor(nrow(data) *.7))
  data.train <- data[idx,]
  data.test <- data[-idx,]
  
  # Build model
  model <- naiveBayes(as.formula(paste(y, " ~ ", x)), data = data.train, laplace = TRUE)
  
  # Test model
  pred <- predict(model, data.test[, names(data.test) != y])
  
  # Evaluate model
  eval.df <- data.frame(actual=data.test[, y], predict=pred)
  accu <- nrow(eval.df[eval.df$actual == eval.df$predict,]) / nrow(eval.df)
  return(paste("Accuracy:", accu))
}

# Naive Bayes imputation
nb.impute <- function(y, x, data) {
  # Build model
  model <- naiveBayes(as.formula(paste(y, " ~ ", x)), data = data, na.action = "na.omit")
  
  # Predict missing values
  data[is.na(data[y]), y] <- predict(model, data[is.na(data[y]),])
  return(data)
}


# Multiple linear regression model selection
r.model.select <- function(y, data) {
  model.df <- data.frame(var=character(), f.stat=numeric(), r2=numeric(),
                         adj.r2=numeric(), mse=numeric(), stringsAsFactors = FALSE)
  c <- 0
  # Generate all regression models of different predictor lengths
  for (i in 1:(ncol(data)-1)) {
    # Generate all combinations of candidate predictors
    comb <- combn(colnames(data[colnames(data) != y]), i)
    
    # Generate statistics for each model
    for (j in 1:ncol(comb)) {
      x <- comb[, j]
      model <- lm(as.formula(paste(y, " ~ ", paste(x, collapse= "+"))),
                  data = data, na.action = "na.omit")
      summ <- summary(model)
      anov <- anova(model)
      c <- c + 1
      model.df[c, "var"] <- paste(x, collapse= "+")
      model.df[c, "f.stat"] <- summ[["fstatistic"]][["value"]]
      model.df[c, "r2"] <- summ[["r.squared"]]
      model.df[c, "adj.r2"] <- summ[["adj.r.squared"]]
      model.df[c, "mse"] <- anov["Residuals", "Mean Sq"]
    }
  }
  return(model.df)
}


# Multiple linear regression model evaluation
r.model.eval <- function(y, x, data) {
  # Create train/test data of complete cases
  data <- na.omit(data)
  set.seed(50)
  idx <- sample(1:nrow(data), floor(nrow(data) *.7))
  data.train <- data[idx,]
  data.test <- data[-idx,]
  
  # Build model
  model <- lm(as.formula(paste(y, " ~ ", x)), data = data.train)
  
  # Test model
  pred <- round(predict(model, data.test[, names(data.test) != y]), 3)
  
  # Evaluate model
  eval.df <- data.frame(actual=data.test[, y], predict=pred)
  plot(eval.df, main=paste(y, " ~ ", x), xlim=c(0,1), ylim=c(0,1))
  abline(a = 0, b = 1)
  mse <- mean((eval.df$actual - eval.df$predict)^2)
  return(paste("MSE:", mse))
}


# Multiple linear regression imputation
r.impute <- function(y, x, data) {
  # Build model
  model <- lm(as.formula(paste(y, " ~ ", x)), data = data, na.action = "na.omit")
  
  # Predict missing values
  data[is.na(data[y]), y] <- round(predict(model, data[is.na(data[y]),]), 3)
  return(data)
}


#### Data Integration ####

# Chi-square test for categorical attributes
chi.test <- function(data) {
  df <- data.frame(var.x=character(), var.y=character(), 
                       stat=numeric(), df=numeric(), p.val=numeric(), stringsAsFactors=FALSE)
  
  # Generate combinations of categorical attribute pairs
  comb <- combn(colnames(data)[sapply(data, is.factor)], 2)
  
  for (i in 1:ncol(comb)) {
    # Perform chi-square test on each attribute pair
    test <- chisq.test(data[, comb[1, i]], data[, comb[2, i]])
    
    # Save test values to data frame
    df[i, "var.x"] <- comb[1, i]
    df[i, "var.y"] <- comb[2, i]
    df[i, "stat"] <- round(test[["statistic"]], 3)
    df[i, "df"] <- test[["parameter"]]
    df[i, "p.val"] <- round(test[["p.value"]], 3)
  }
  return(df)
}