# Import packages
library(e1071)  # naive Bayes

# Import data
data.orig <- read.csv("data/credit_approval.csv", header = FALSE, na.strings = "?")
data <- data.orig


#### Data Exploration ####

# Generate frequency tables and summary statistics for all attributes
summary(data)


#### Data Transformation ####

# Normalize numeric attributes using min-max [0,1]
for (i in 1:ncol(data)) {
  if (is.numeric(data[, i])) {
    data[, i] <- round((data[, i] - min(data[, i], na.rm = TRUE)) / 
      (max(data[, i], na.rm = TRUE) - min(data[, i], na.rm = TRUE)), 3)
  }
}


#### Data Integration ####

# Check for redundant categorical attributes using chi-square test
chi.df <- data.frame(var.x=character(), var.y=character(), 
                     stat=numeric(), df=numeric(), p.val=numeric(), stringsAsFactors=FALSE)

# Generate combinations of categorical attribute pairs
chi.comb <- combn(colnames(data)[sapply(data, is.factor)], 2)

for (i in 1:ncol(chi.comb)) {
  # Perform chi-square test on each attribute pair
  chi.test <- chisq.test(data[, chi.comb[1, i]], data[, chi.comb[2, i]])
  
  # Save test values to data frame
  chi.df[i, "var.x"] <- chi.comb[1, i]
  chi.df[i, "var.y"] <- chi.comb[2, i]
  chi.df[i, "stat"] <- round(chi.test[["statistic"]], 3)
  chi.df[i, "df"] <- chi.test[["parameter"]]
  chi.df[i, "p.val"] <- round(chi.test[["p.value"]], 3)
}

# Remove redundant categorical attributes that are not highly correlated to class
data[, "V7"] <- NULL  # Correlated with V6 but also correlated to class
data[, "V5"] <- NULL  # Correlated with V4
data[, "V13"] <- NULL  # Correlated with V4
# data[, "V10"] <- NULL  # Correlated with V9 but also correlated to class
data[, "V1"] <- NULL  # Correlated with V6
# data[, "V6"] <- NULL  # Correlated with V9 but also correlated to class

# Remove categorical attributes uncorrelated to class
data[, "V12"] <- NULL


# Check for redundant numerical attributes using correlation coefficient
cor.df <- data.frame(var.x=character(), var.y=character(), r=numeric(), stringsAsFactors=FALSE)

# Generate combinations of numerical attribute pairs
cor.comb <- combn(colnames(data)[sapply(data, is.numeric)], 2)

# Calculate correlation coefficient of each attribute pair
for (i in 1:ncol(cor.comb)) {
  cor.df[i, "var.x"] <- cor.comb[1, i]
  cor.df[i, "var.y"] <- cor.comb[2, i]
  cor.df[i, "r"] <- round(cor(data[, cor.comb[1, i]], data[, cor.comb[2, i]], 
                              use = "complete.obs"), 3)
}


#### Data Cleaning ####

# Check for missing values
anyNA(data)

# Fill in missing values for categorical attributes using naive Bayes
nb.model <- naiveBayes(V6 ~ ., data = data, na.action = "na.omit")
data2[540, "V6"] <- predict(nb.model, data[540,])



# Fill in missing values for numeric attribute V2 using regression
r.model.v2.df <- r.model.select("V2", data) # Generate all regression models

# Find model with largest R^2 and adjusted R^2, and smallest MSE
head(r.model.v2.df[order(r.model.v2.df$r2, decreasing = TRUE),], n = 16)
head(r.model.v2.df[order(r.model.v2.df$adj.r2, decreasing = TRUE),], n = 12)
head(r.model.v2.df[order(r.model.v2.df$mse),])
r.model.v2.1 <- "V3+V4+V6+V8+V9+V10+V11+V14"  # Model for NAs only in V2
r.model.v2.2 <- "V3+V4+V6+V8+V9+V10+V11"  # Model for NAs in V2 and V14

data <- r.impute("V2", r.model.v2.1, data)
data <- r.impute("V2", r.model.v2.2, data)


# Fill in missing values for numeric attribute V14 using regression
r.model.v14.df <- r.model.select("V14", data)


# Regression imputation
r.impute <- function(y, x, data) {
  model <- lm(as.formula(paste(y, " ~ ", x)), data = data, na.action = "na.omit")
  data[is.na(data[y]), y] <- round(predict(model, data[is.na(data[y]),]), 3)
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



# Check for duplicates
anyDuplicated(data)

# Check for errors in categorical attributes
for (i in 1:ncol(data)) {
  if (is.factor(data[, i])) {
    print(colnames(data)[i])
    print(levels(data[, i]))
  }
}

# Check for outliers
boxplot(data[sapply(data, is.numeric)])

# Smooth outliers using bin means


#### Data Reduction ####

