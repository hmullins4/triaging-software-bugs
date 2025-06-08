# Load packages

library(ROCR)
library(dplyr)
library(caret)
library(stargazer)
library(pROC)
library(RColorBrewer)

# Read in data.

eclipse_data <- read.csv("ECO4934_SUMMER24_FIN/Data/Eclipse_Data.csv", sep = ',', header = TRUE)

eclipse_data <- subset(eclipse_data, select = -id)

# Observe variables contained within.

names(eclipse_data)

# The dependent variable, curr_res, refers
# to the current resolution of a bug with values of either
# "FIXED" or "WONTFIX" which we will convert into 0s and 1s.
# Our other variables consist of


#1. reporter: The individual sharing an update about a bug at
# a particular time

#2. stat_upd: The number of times a bug's status was updated

#3. num_intrst: The number of individuals interested in the
# progress of a bug

#4. op_sys: The operating system against which the bug is reported

#5. component: The relevant subsystem of the product for the reported bug

#6. prod: The specific software application the bug is related to

#7. severity: An indication of the impact of the bug on the software system

#8. version: The version of the product the bug was found in

#9. times_assigned: The number of times a bug was assigned (thus
# indicating how many times it was reassigned)

#10. succ_rate: The ratio of a reporter's total "FIXED" updates to
# his/her total updates

#11. res_upd: The number of times a bug was updated from initial entry
# to resolution

#12. res_time: The time elapsed between a bug's initial entry
# to resolution

#13. reporter_report_cnt: The number of times a particular reporter
# entered updates on bugs.

#14. desc_length: From the short_desc.csv file, the total number of
# characters from each bug's description(s)

#15. prio: Indicates how soon a bug should be fixed



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

# Begin with logit model.

set.seed(123456)
logit_model <- glm(curr_res ~., family = binomial(link = "logit"), data = train_poly)

summary(logit_model)

# Create predictions for training set.

pred_prob_train_logit <- predict.glm(logit_model, type = "response")
pred_train_logit <- prediction(pred_prob_train_logit, train_poly$curr_res)
perf_train_logit <- performance(pred_train_logit, "tpr", "fpr")

# Calculate area under the curve.

unlist(slot(performance(pred_train_logit, "auc"), "y.values"))

# Create predictions for testing set.

pred_prob_test_logit <- predict.glm(logit_model, newdata = test_poly, type = "response")
pred_test_logit <- prediction(pred_prob_test_logit, test_poly$curr_res)
perf_test_logit <- performance(pred_test_logit, "tpr", "fpr")

# Calculate area under the curve.

unlist(slot(performance(pred_test_logit, "auc"), "y.values"))


# To determine the optimal threshold (that which gives us the
# greatest accuracy), create a function that runs through a sequence
# of thresholds. From this, we might also calculate the true positive
# rate, the true negative rate, the false positive rate, and the
# false negative rate at the optimal threshold.


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
  perf_logit <- calculate_performance(pred_prob_test_logit, test_poly$curr_res, thresholds[i])
  sensitivity[i] <- perf_logit$sensitivity
  specificity[i] <- perf_logit$specificity
  accuracy[i] <- perf_logit$accuracy
}

best_accuracy_logit <- max(accuracy)
best_sensitivity_logit <- sensitivity[which.max(accuracy)]
best_specificity_logit <- specificity[which.max(accuracy)]

best_threshold_logit <- thresholds[which.max(accuracy)]
best_threshold_logit

# Plot the ROC Curve.

plot(perf_test_logit, main = "ROC Curve Logit", colorize = TRUE)
legend("bottomright", legend = paste("Threshold =", round(best_threshold_logit, 2)), col = "black", lty = 1)
abline(a=0,b=1)


# Now, calculate the TPR, TNR, FPR, and FNR.

class_prediction_logit <- ifelse(pred_prob_test_logit > best_threshold_logit, "positive_class", "negative_class")

y <- table(test_poly$curr_res)
table(class_prediction)


confmat_logit <- table(class_prediction_logit, test_poly$curr_res)
confmat_logit

tpr_logit <- confmat_logit[2,2]/y[[2]]
fpr_logit <- confmat_logit[2,1]/y[[1]]
tnr_logit <- confmat_logit[1,1]/y[[1]]
fnr_logit <- confmat_logit[1,2]/y[[2]]

tpr_logit
fpr_logit
tnr_logit
fnr_logit
