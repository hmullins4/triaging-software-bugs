# Load packages

library(ROCR)
library(dplyr)
library(caret)
library(stargazer)
library(pROC)
library(RColorBrewer)
library(gbm)

#  Read in data.

eclipse_data <- read.csv("ECO4934_SUMMER24_FIN/Data/Eclipse_Data.csv", sep = ',', header = TRUE)

eclipse_data <- subset(eclipse_data, select = -id)

# Observe variables contained within.

names(eclipse_data)

# Ensure variables are properly formatted.

for (i in 1:length(eclipse_data$curr_res)) {
  if (eclipse_data$curr_res[i] == "FIXED") {
    eclipse_data$curr_res[i] <- 1
  } else {
    eclipse_data$curr_res[i] <- 0
  }
}

eclipse_data$curr_res <- as.factor(eclipse_data$curr_res)
eclipse_data$op_sys <- as.numeric(factor(eclipse_data$op_sys))
eclipse_data$prod <- as.numeric(factor(eclipse_data$prod))
eclipse_data$component <- as.numeric(factor(eclipse_data$component))
eclipse_data$version <- as.numeric(factor(eclipse_data$version))
eclipse_data$severity <- as.numeric(factor(eclipse_data$severity))

# Generate summary statistics.

stargazer(eclipse_data, type = 'text')

# Split the data into training and testing sets.

set.seed(123456)
index <- sample(nrow(eclipse_data), nrow(eclipse_data)*.60)
train <- eclipse_data[index,]
test <- eclipse_data[-index,]

# Create new variables (polynomial and interaction variables)

train_poly <- as.data.frame(poly(as.matrix(train[, -which(names(train) == "curr_res")]), degree = 2, raw = TRUE))

train_poly$curr_res <- train$curr_res

test_poly <- as.data.frame(poly(as.matrix(test[, -which(names(test) == "curr_res")]), degree =2, raw = TRUE))

test_poly$curr_res <- test$curr_res

# Boosting Model

train_poly$curr_res <- as.numeric(as.character(train_poly$curr_res))
test_poly$curr_res <- as.numeric(as.character(test_poly$curr_res))

set.seed(123456)
boost_model <- gbm(curr_res ~., distribution = "bernoulli", data = train_poly, n.trees = 10000, shrinkage = .1, cv.folds = 10, n.cores = 30)

boost_model

# Predictions with training data.

pred_prob_train_boost <- predict(boost_model, type = "response")

pred_train_boost <- prediction(pred_prob_train_boost, train_poly$curr_res)

perf_train_boost <- performance(pred_train_boost, "tpr", "fpr")

# AUC

unlist(slot(performance(pred_train_boost, "auc"), "y.values"))

# Predictions with testing data.

pred_prob_test_boost <- predict(boost_model, newdata = test_poly, type = "response")

pred_test_boost <- prediction(pred_prob_test_boost, test_poly$curr_res)

perf_test_boost <- performance(pred_test_boost, "tpr", "fpr")

# AUC

unlist(slot(performance(pred_test_boost, "auc"), "y.values"))


# Determine the optimal threshold for boost_model.

calculate_performance <- function(scores, actual, threshold) {
  predicted <- ifelse(scores > threshold, 1, 0)
  cm <- table(predicted, actual)
  
  if (nrow(cm) < 2 || ncol(cm) < 2) {
    TP <- TN <- FP <- FN <- 0
  } else {
    TP <- cm[2, 2]
    FP <- cm[1, 2]
    TN <- cm[1, 1]
    FN <- cm[2, 1]
  }
  
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  accuracy <- (TP + TN) / sum(cm)
  
  return(list(sensitivity = sensitivity, specificity = specificity, accuracy = accuracy))
}

thresholds <- seq(0.01, 0.99, by = 0.01)
sensitivity <- numeric(length(thresholds))
specificity <- numeric(length(thresholds))
accuracy <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  perf_boost <- calculate_performance(pred_prob_test_boost, test_poly$curr_res, thresholds[i])
  sensitivity[i] <- perf_boost$sensitivity
  specificity[i] <- perf_boost$specificity
  accuracy[i] <- perf_boost$accuracy
}

best_accuracy_boost <- max(accuracy)
best_sensitivity_boost <- sensitivity[which.max(accuracy)]
best_specificity_boost <- specificity[which.max(accuracy)]

best_threshold_boost <- thresholds[which.max(accuracy)]
best_threshold_boost

# Plot the ROC curve.

plot(perf_test_boost, main = "ROC Curve Boosting", colorize = TRUE)
legend("bottomright", legend = paste("Threshold =", round(best_threshold_boost, 2)), col = "black", lty = 1)
abline(a=0,b=1)

# Now, calculate the TPR, TNR, FPR, and FNR.

class_prediction_boost <- ifelse(pred_prob_test_boost > best_threshold_boost, "positive_class", "negative_class")

y <- table(test_poly$curr_res)
table(class_prediction_boost)

confmat_boost <- table(class_prediction_boost, test_poly$curr_res)
confmat_boost

tpr_boost <- confmat_boost[2,2]/y[[2]]
fpr_boost <- confmat_boost[2,1]/y[[1]]
tnr_boost <- confmat_boost[1,1]/y[[1]]
fnr_boost <- confmat_boost[1,2]/y[[2]]

tpr_boost
fpr_boost
tnr_boost
fnr_boost

















