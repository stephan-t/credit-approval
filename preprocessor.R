# Import functions
source("functions.R")

# Import data
data <- read.csv("data/credit_approval.csv", header = FALSE, na.strings = "?")


#### Data Exploration ####

# Check first few records of data set
head(data)

# Generate frequency tables and summary statistics for all attributes
summary(data)


#### Data Transformation ####

# Normalize numeric attributes using min-max [0,1]
for (i in 1:ncol(data)) {
  if (is.numeric(data[, i])) {
    data[, i] <- round(((data[, i] - min(data[, i], na.rm = TRUE)) / 
      (max(data[, i], na.rm = TRUE) - min(data[, i], na.rm = TRUE))) *
        (1.0 - 0.0) + 0.0, 3)
  }
}


#### Data Cleaning ####

# Check for missing values
anyNA(data)

# Prepare to impute missing values for categorical attributes using naive Bayes
(chi.df <- chi.test(data))  # Find correlated attributes to use in prediction

# Find models with highest prediction accuracies
nb.model.v1 <- list("V1", "V6")
nb.model.eval(nb.model.v1[[1]], nb.model.v1[[2]], data)
nb.model.v4 <- list("V4", "V5")
nb.model.eval(nb.model.v4[[1]], nb.model.v4[[2]], data)
nb.model.v5 <- list("V5", "V4")
nb.model.eval(nb.model.v5[[1]], nb.model.v5[[2]], data)
nb.model.v6 <- list("V6", "V7+V16")
nb.model.eval(nb.model.v6[[1]], nb.model.v6[[2]], data)
nb.model.v7 <- list("V7", "V6")
nb.model.eval(nb.model.v7[[1]], nb.model.v7[[2]], data)

# Impute missing values
data <- nb.impute(nb.model.v1[[1]], nb.model.v1[[2]], data)
data <- nb.impute(nb.model.v4[[1]], nb.model.v4[[2]], data)
data <- nb.impute(nb.model.v5[[1]], nb.model.v5[[2]], data)
data <- nb.impute(nb.model.v6[[1]], nb.model.v6[[2]], data)
data <- nb.impute(nb.model.v7[[1]], nb.model.v7[[2]], data)


# Prepare to impute missing values for numeric attribute V2 using regression
time.start <- Sys.time()
r.model.v2.df <- r.model.select("V2", data)  # Generate all regression models
(time.end <- Sys.time() - time.start)  # ~3 mins at 4GHz

# Find model with largest R^2 and adjusted R^2, and smallest MSE with NAs in V2 only
head(r.model.v2.df[order(r.model.v2.df$r2, decreasing = TRUE),])
head(r.model.v2.df[order(r.model.v2.df$adj.r2, decreasing = TRUE),])
head(r.model.v2.df[order(r.model.v2.df$mse),])
r.model.v2.1 <- list("V2", "V1+V3+V4+V7+V8+V9+V10+V11+V12+V14")
r.model.eval(r.model.v2.1[[1]], r.model.v2.1[[2]], data)

# Find model with largest R^2 and adjusted R^2, and smallest MSE with NAs in V2 and V14
# excluding V5 & V13 due to multicollinearity
r.model.v2.df <- r.model.v2.df[order(r.model.v2.df$r2, decreasing = TRUE),]
head(r.model.v2.df[!grepl("(V14|V5|V13)", r.model.v2.df$var),])
r.model.v2.df <- r.model.v2.df[order(r.model.v2.df$adj.r2, decreasing = TRUE),]
head(r.model.v2.df[!grepl("(V14|V5|V13)", r.model.v2.df$var),])
r.model.v2.df <- r.model.v2.df[order(r.model.v2.df$mse),]
head(r.model.v2.df[!grepl("(V14|V5|V13)", r.model.v2.df$var),])
r.model.v2.2 <- list("V2", "V4+V6+V7+V8+V9+V10+V11+V15")
r.model.eval(r.model.v2.2[[1]], r.model.v2.2[[2]], data)

# Impute missing values
data <- r.impute(r.model.v2.1[[1]], r.model.v2.1[[2]], data)
data <- r.impute(r.model.v2.2[[1]], r.model.v2.2[[2]], data)

# Prepare to impute missing values for numeric attribute V14 using regression
time.start <- Sys.time()
r.model.v14.df <- r.model.select("V14", data)  # Generate all regression models
(time.end <- Sys.time() - time.start)  # ~3 mins at 4GHz

# Find model with largest R^2 and adjusted R^2, and smallest MSE with NAs in V14 only
head(r.model.v14.df[order(r.model.v14.df$r2, decreasing = TRUE),])
head(r.model.v14.df[order(r.model.v14.df$adj.r2, decreasing = TRUE),])
head(r.model.v14.df[order(r.model.v14.df$mse),])
r.model.v14 <- list("V14", "V3+V6+V7+V10+V11+V12+V13+V15+V16")
r.model.eval(r.model.v14[[1]], r.model.v14[[2]], data)

# Impute missing values
data <- r.impute(r.model.v14[[1]], r.model.v14[[2]], data)

# Check for missing values
anyNA(data)

# Check for duplicates
anyDuplicated(data)

# Check for errors in categorical attributes
for (i in 1:ncol(data)) {
  if (is.factor(data[, i])) {
    cat(colnames(data)[i], levels(data[, i]), "\n")
  }
}

# Check for outliers
boxplot(data[sapply(data, is.numeric)])
outlier <- outlier.detect(data)

# Delete major outliers
outlier.del <- c()
for (i in names(outlier)) {
  if (length(outlier[[i]][["maj.up"]]) != 0) {
    outlier.del <- append(outlier.del, as.numeric(outlier[[i]][["maj.up"]]))
  }
}
data <- data[-c(sort(unique(outlier.del))),]
rownames(data) <- 1:nrow(data)
boxplot(data[sapply(data, is.numeric)])


#### Data Reduction ####

# Check for redundant categorical attributes using chi-square test
(chi.df <- chi.test(data))

# Remove redundant categorical attributes that are not highly correlated to class
data[, "V7"] <- NULL  # Correlated with V6
data[, "V5"] <- NULL  # Correlated with V4
data[, "V13"] <- NULL  # Correlated with V4
data[, "V1"] <- NULL  # Correlated with V6

# Remove irrelevant categorical attributes that are uncorrelated to class
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
cor.df