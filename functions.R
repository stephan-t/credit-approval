# Import packages
library(e1071)  # naive Bayes


#### Data Cleaning ####

# Naive Bayes model evaluation
nb.model.eval <- function(y, x, data) {
  # Split data for training and testing using complete cases
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
  # Split data for training and testing using complete cases
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


# Outlier detection
outlier.detect <- function(data) {
  outliers <- list()
  
  for (i in 1:ncol(data)) {
    if (is.numeric(data[, i])) {
      var <- colnames(data)[i]
      
      # Calculate minor outliers
      fence.min.low <- quantile(data[, i], na.rm = TRUE)[2] - (IQR(data[, i], na.rm = TRUE) * 1.5)
      fence.min.up <- quantile(data[, i], na.rm = TRUE)[4] + (IQR(data[, i], na.rm = TRUE) * 1.5)
      out.min.low <- data[data[, i] < fence.min.low,]
      out.min.up <- data[data[, i] > fence.min.up,]
      
      # Calculate major outliers
      fence.maj.low <- quantile(data[, i], na.rm = TRUE)[2] - (IQR(data[, i], na.rm = TRUE) * 3)
      fence.maj.up <- quantile(data[, i], na.rm = TRUE)[4] + (IQR(data[, i], na.rm = TRUE) * 3)
      out.maj.low <- data[data[, i] < fence.maj.low,]
      out.maj.up <- data[data[, i] > fence.maj.up,]
      
      # Store indexes of outliers 
      outliers[[var]][["min.low"]] <- rownames(out.min.low)
      outliers[[var]][["min.up"]] <- rownames(out.min.up)
      outliers[[var]][["maj.low"]] <- rownames(out.maj.low)
      outliers[[var]][["maj.up"]] <- rownames(out.maj.up)
      
      # Print results
      cat(var, nrow(out.min.low), nrow(out.min.up), nrow(out.maj.low), nrow(out.maj.up), "\n")
    }
  }
  return(outliers)
}


#### Data Reduction ####

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
    df[i, "p.val"] <- test[["p.value"]]
  }
  return(df)
}


# Correlation coefficient for numerical attributes
cor.co <- function(data) {
  df <- data.frame(var.x=character(), var.y=character(), r=numeric(), stringsAsFactors=FALSE)
  
  # Generate combinations of numerical attribute pairs
  comb <- combn(colnames(data)[sapply(data, is.numeric)], 2)
  
  # Calculate correlation coefficient of each attribute pair
  for (i in 1:ncol(comb)) {
    df[i, "var.x"] <- comb[1, i]
    df[i, "var.y"] <- comb[2, i]
    df[i, "r"] <- round(cor(data[, comb[1, i]], data[, comb[2, i]], use = "complete.obs"), 3)
  }
  return(df)  
}


#### Data Transformation ####

# Normalize numeric attributes using min-max
norm.minmax <- function(data, new.min, new.max) {
  for (i in 1:ncol(data)) {
    if (is.numeric(data[, i])) {
      data[, i] <- round(((data[, i] - min(data[, i], na.rm = TRUE)) /
        (max(data[, i], na.rm = TRUE) - min(data[, i], na.rm = TRUE))) *
          (new.max - new.min) + new.min, 3)
    }
  }
  return(data)
}
 
 
# Normalize numeric attributes using z-score
norm.zscore <- function(data, mad = FALSE) {
  for (i in 1:ncol(data)) {
    if (is.numeric(data[, i])) {
      if (mad == TRUE) {
        # Calculate using mean absolute deviation
        data[, i] <- round(((data[, i] - mean(data[, i], na.rm = TRUE)) / 
                              (sum(abs(data[, i] - mean(data[, i], na.rm = TRUE)) 
                               / length(na.omit(data[, i])), na.rm = TRUE))), 3)
      } else {
        # Calculate using population standard deviation
        data[, i] <- round(((data[, i] - mean(data[, i], na.rm = TRUE)) / 
                              (sqrt((length(na.omit(data[, i])) - 1) / length(na.omit(data[, i]))) * 
                                 sd(data[, i], na.rm = TRUE))), 3)
      }
    }
  }
  return(data)
}