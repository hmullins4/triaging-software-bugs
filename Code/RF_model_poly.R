# Load packages

library(ROCR)
library(dplyr)
library(caret)
library(stargazer)
library(pROC)
library(RColorBrewer)
library(randomForest)

# Read in data.

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


# Random Forest Model

colnames(train_poly) <- paste0("V", seq_along(colnames(train_poly)))

train_poly <- rename(train_poly, curr_res = V136)

colnames(test_poly) <- paste0("V", seq_along(colnames(test_poly)))

test_poly <- rename(test_poly, curr_res = V136)

set.seed(123456)
RF_model <- randomForest(curr_res ~., data = train_poly, ntree = 2000, mtry = 5)

RF_model
summary(RF_model)

# Obtain predictions with training data.

pred_prob_train_RF <- predict(RF_model, type = "prob")[,2]
pred_train_RF <- prediction(pred_prob_train_RF, train_poly$curr_res)
perf_train_RF <- performance(pred_train_RF, "tpr", "fpr")

# AUC

unlist(slot(performance(pred_train_RF, "auc"), "y.values"))

# Obtain predictions with testing data.

pred_prob_test_RF <- predict(RF_model, newdata = test_poly, type = "prob")[,2]
pred_test_RF <- prediction(pred_prob_test_RF, test_poly$curr_res)
perf_test_RF <- performance(pred_test_RF, "tpr", "fpr")

# AUC

unlist(slot(performance(pred_test_RF, "auc"), "y.values"))

# Determine the optimal threshold for RF_model.

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
  perf_RF <- calculate_performance(pred_prob_test_RF, test_poly$curr_res, thresholds[i])
  sensitivity[i] <- perf_RF$sensitivity
  specificity[i] <- perf_RF$specificity
  accuracy[i] <- perf_RF$accuracy
}

best_accuracy_RF <- max(accuracy)
best_sensitivity_RF <- sensitivity[which.max(accuracy)]
best_specificity_RF <- specificity[which.max(accuracy)]

best_threshold_RF <- thresholds[which.max(accuracy)]
best_threshold_RF

# Plot the ROC curve.

plot(perf_test_RF, main = "ROC Curve Random Forest", colorize = TRUE)
legend("bottomright", legend = paste("Threshold =", round(best_threshold_RF, 2)), col = "black", lty = 1)
abline(a=0,b=1)

# Now, calculate the TPR, TNR, FPR, and FNR.

class_prediction_RF <- ifelse(pred_prob_test_RF > best_threshold_RF, "positive_class", "negative_class")

y <- table(test_poly$curr_res)
table(class_prediction_RF)


confmat_RF <- table(class_prediction_RF, test_poly$curr_res)
confmat_RF

tpr_RF <- confmat_RF[2,2]/y[[2]]
fpr_RF <- confmat_RF[2,1]/y[[1]]
tnr_RF <- confmat_RF[1,1]/y[[1]]
fnr_RF <- confmat_RF[1,2]/y[[2]]

tpr_RF
fpr_RF
tnr_RF
fnr_RF




















