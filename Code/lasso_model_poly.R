# Load packages

library(ROCR)
library(dplyr)
library(caret)
library(stargazer)
library(pROC)
library(RColorBrewer)
library(glmnet)

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

# Begin LASSO models.

set.seed(123456)
LASSO_model_min <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.min)

set.seed(123456)
LASSO_model_1se <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.1se)

summary(LASSO_model_min)

summary(LASSO_model_1se)

# Create predictions with training data.

pred_prob_train_LASSO_min <- predict(LASSO_model_min, x, s = "lambda.min", type = "response")
pred_train_LASSO_min <- prediction(pred_prob_train_LASSO_min, train_poly$curr_res)
perf_train_LASSO_min <- performance(pred_train_LASSO_min, "tpr", "fpr")


pred_prob_train_LASSO_1se <- predict(LASSO_model_1se, x, s = "lambda.1se", type = "response")
pred_train_LASSO_1se <- prediction(pred_prob_train_LASSO_1se, train_poly$curr_res)
perf_train_LASSO_1se <- performance(pred_train_LASSO_1se, "tpr", "fpr")

# AUC

unlist(slot(performance(pred_train_LASSO_min, "auc"), "y.values"))

unlist(slot(performance(pred_train_LASSO_1se, "auc"), "y.values"))

# Predictions with test data.

x.test <- model.matrix(curr_res ~., data = test_poly)[,-1]

pred_prob_test_LASSO_min <- predict(LASSO_model_min, newx = x.test, s = "lambda.min", type = "response")
pred_test_LASSO_min <- prediction(pred_prob_test_LASSO_min, test_poly$curr_res)
perf_test_LASSO_min <- performance(pred_test_LASSO_min, "tpr", "fpr")

pred_prob_test_LASSO_1se <- predict(LASSO_model_1se, newx = x.test, s = "lambda.1se", type = "response")
pred_test_LASSO_1se <- prediction(pred_prob_test_LASSO_1se, test_poly$curr_res)
perf_test_LASSO_1se <- performance(pred_test_LASSO_1se, "tpr", "fpr")

# AUC

unlist(slot(performance(pred_test_LASSO_min, "auc"), "y.values"))

unlist(slot(performance(pred_test_LASSO_1se, "auc"), "y.values"))

# Determine the optimal threshold for LASSO_min.

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
  perf_LASSO_min <- calculate_performance(pred_prob_test_LASSO, test_poly$curr_res, thresholds[i])
  sensitivity[i] <- perf_LASSO_min$sensitivity
  specificity[i] <- perf_LASSO_min$specificity
  accuracy[i] <- perf_LASSO_min$accuracy
}

best_accuracy_LASSO_min <- max(accuracy)
best_sensitivity_LASSO_min <- sensitivity[which.max(accuracy)]
best_specificity_LASSO_min <- specificity[which.max(accuracy)]


best_threshold_LASSO_min <- thresholds[which.max(accuracy)]
best_threshold_LASSO_min

# Plot the ROC curve.

plot(perf_test_LASSO_min, main = "ROC Curve Lasso (Minimum Lambda Value)", colorize = TRUE)
legend("bottomright", legend = paste("Threshold =", round(best_threshold_LASSO_min, 2)), col = "black", lty = 1)
abline(a=0,b=1)

# Now, calculate the TPR, TNR, FPR, and FNR.

class_prediction_LASSO_min <- ifelse(pred_prob_test_LASSO_min > best_threshold_LASSO_min, "positive_class", "negative_class")

y <- table(test_poly$curr_res)
table(class_prediction_LASSO_min)


confmat_LASSO_min <- table(class_prediction_LASSO_min, test_poly$curr_res)
confmat_LASSO_min

tpr_LASSO_min <- confmat_LASSO_min[2,2]/y[[2]]
fpr_LASSO_min <- confmat_LASSO_min[2,1]/y[[1]]
tnr_LASSO_min <- confmat_LASSO_min[1,1]/y[[1]]
fnr_LASSO_min <- confmat_LASSO_min[1,2]/y[[2]]

tpr_LASSO_min
fpr_LASSO_min
tnr_LASSO_min
fnr_LASSO_min


# Determine the optimal threshold for LASSO_1se.

thresholds <- seq(0.01, 0.99, by = 0.01)
sensitivity <- numeric(length(thresholds))
specificity <- numeric(length(thresholds))
accuracy <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  perf_LASSO_1se <- calculate_performance(pred_prob_test_LASSO_1se, test_poly$curr_res, thresholds[i])
  sensitivity[i] <- perf_LASSO_1se$sensitivity
  specificity[i] <- perf_LASSO_1se$specificity
  accuracy[i] <- perf_LASSO_1se$accuracy
}

best_accuracy_LASSO_1se <- max(accuracy)
best_sensitivity_LASSO_1se <- sensitivity[which.max(accuracy)]
best_specificity_LASSO_1se <- specificity[which.max(accuracy)]


best_threshold_LASSO_1se <- thresholds[which.max(accuracy)]
best_threshold_LASSO_1se

# Plot the ROC curve.

plot(perf_test_LASSO_1se, main = "ROC Curve Lasso (One Standard Error Lambda Value)", colorize = TRUE)
legend("bottomright", legend = paste("Threshold =", round(best_threshold_LASSO_1se, 2)), col = "black", lty = 1)
abline(a=0,b=1)

# Now, calculate the TPR, TNR, FPR, and FNR.

class_prediction_LASSO_1se <- ifelse(pred_prob_test_LASSO_1se > best_threshold_LASSO_1se, "positive_class", "negative_class")

y <- table(test_poly$curr_res)
table(class_prediction_LASSO_1se)


confmat_LASSO_1se <- table(class_prediction_LASSO_1se, test_poly$curr_res)
confmat_LASSO_1se

tpr_LASSO_1se <- confmat_LASSO_1se[2,2]/y[[2]]
fpr_LASSO_1se <- confmat_LASSO_1se[2,1]/y[[1]]
tnr_LASSO_1se <- confmat_LASSO_1se[1,1]/y[[1]]
fnr_LASSO_1se <- confmat_LASSO_1se[1,2]/y[[2]]

tpr_LASSO_1se
fpr_LASSO_1se
tnr_LASSO_1se
fnr_LASSO_1se

























