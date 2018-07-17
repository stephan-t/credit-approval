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




#### Data Integration ####

# Check for redundant categorical attributes using chi-square test
cst.df <- data.frame(var.x=character(), var.y=character(), 
                     stat=numeric(), df=numeric(), p.val=numeric(), stringsAsFactors=FALSE)

# Generate combinations of categorical attribute pairs
f <- sapply(data[0, -16], is.factor)
cmb <- combn(colnames(data[-16])[f], 2)

for (i in 1:ncol(cmb)) {
  # Perform chi-square test on attribute pair
  cst <- chisq.test(data[, cmb[1, i]], data[, cmb[2, i]])
  
  # Save test values to data frame
  cst.df[i, "var.x"] <- cmb[1, i]
  cst.df[i, "var.y"] <- cmb[2, i]
  cst.df[i, "stat"] <- round(cst[["statistic"]], 3)
  cst.df[i, "df"] <- cst[["parameter"]]
  cst.df[i, "p.val"] <- round(cst[["p.value"]], 6)
}

chi <- chisq.test(data$V6, data$V7)

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
nb.model <- naiveBayes(V6 ~ ., data = data, na.action = "na.omit")
predict(nb.model, data[540, -6])

# Fill in missing values for numeric attributes using regression
l.model <- lm(V2 ~ V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 
              + V11 + V12 + V13 + V14 + V15 + V16, data = data, na.action = "na.omit")
predict(l.model, data[93,])
plot(l.model)
