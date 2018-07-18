# Import packages
library(e1071)

# Import data
data.orig <- read.csv("data/credit_approval.csv", header = FALSE, na.strings = "?")
data <- data.orig


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
chi.cmb <- combn(colnames(data)[sapply(data[0,], is.factor)], 2)

for (i in 1:ncol(chi.cmb)) {
  # Perform chi-square test on each attribute pair
  chi.test <- chisq.test(data[, chi.cmb[1, i]], data[, chi.cmb[2, i]])
  
  # Save test values to data frame
  chi.df[i, "var.x"] <- chi.cmb[1, i]
  chi.df[i, "var.y"] <- chi.cmb[2, i]
  chi.df[i, "stat"] <- round(chi.test[["statistic"]], 3)
  chi.df[i, "df"] <- chi.test[["parameter"]]
  chi.df[i, "p.val"] <- round(chi.test[["p.value"]], 3)
}

# Remove redundant categorical attributes that are not highly correlated to class
data[, "V7"] <- NULL  # Correlated with V6 but also correlated to class (test with/without)
data[, "V5"] <- NULL  # Correlated with V4
data[, "V13"] <- NULL  # Correlated with V4
# data[, "V10"] <- NULL  # Correlated with V9 but also correlated to class (test with/without)
data[, "V1"] <- NULL  # Correlated with V6
# data[, "V6"] <- NULL  # Correlated with V9 but also correlated to class (test with/without)

# Remove categorical attribute uncorrelated to class
data[, "V12"] <- NULL


# Check for redundant numerical attributes using correlation coefficient
cor.df <- data.frame(var.x=character(), var.y=character(), r=numeric(), stringsAsFactors=FALSE)

# Generate combinations of numerical attribute pairs
cor.cmb <- combn(colnames(data)[sapply(data[0,], is.numeric)], 2)

for (i in 1:ncol(cor.cmb)) {
  # Calculate correlation coefficient of each attribute pair
  cor.df[i, "var.x"] <- cor.cmb[1, i]
  cor.df[i, "var.y"] <- cor.cmb[2, i]
  cor.df[i, "r"] <- cor(data[, cor.cmb[1, i]], data[, cor.cmb[2, i]], use = "complete.obs")
}




#### Data Cleaning ####

# Check for duplicates
anyDuplicated(data)

# Check for errors in categorical attributes
for (i in 1:ncol(data)) {
  if (is.factor(data[, i])) {
    print(colnames(data)[i])
    print(levels(data[, i]))
  }
}

# Check for missing values
anyNA(data)

# Fill in missing values for categorical attributes using naive Bayes
nb.model <- naiveBayes(V6 ~ ., data = data, na.action = "na.omit")
predict(nb.model, data[540, -6])

# Fill in missing values for numeric attributes using regression
l.model <- lm(V2 ~ V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 
              + V11 + V12 + V13 + V14 + V15 + V16, data = data, na.action = "na.omit")
predict(l.model, data[93,])
plot(l.model)
