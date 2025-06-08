# Clear out any old information/data, if necessary
rm(list = ls())

# Import packages.
library(ROCR)
library(dplyr)
library(caret)
library(stargazer)
library(glmnet)
library(randomForest)
library(gbm)
library(pROC)
library(ipred)
library(rpart)
library(RColorBrewer)

# Next, read in the data (our team edited much of this using 
# sqlite3 in VSCode). The dependent variable, curr_res, refers
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


# Having established which covariates will be used in determining
# our best prediction model, read in the data.

eclipse_data <- read.csv("C:/ucf_classes/eco_4934/data/Eclipse_Data.csv", sep=',', header=TRUE)
eclipse_data <- subset(eclipse_data, select = -id)

# Now, let's convert our dependent variable into numerical values.

for (i in 1:length(eclipse_data$curr_res)) {
  if (eclipse_data$curr_res[i] == "FIXED") {
      eclipse_data$curr_res[i] <- 1
    } else {
      eclipse_data$curr_res[i] <- 0
    }
}

eclipse_data$curr_res <- as.factor(eclipse_data$curr_res)


# Because some of the covariates are not numerical, we will
# convert them here.


eclipse_data$op_sys <- as.numeric(factor(eclipse_data$op_sys))
eclipse_data$component <- as.numeric(factor(eclipse_data$component))
eclipse_data$prod <- as.numeric(factor(eclipse_data$prod))
eclipse_data$version <- as.numeric(factor(eclipse_data$version))
eclipse_data$severity <- as.numeric(factor(eclipse_data$severity))


# To understand the data a little better, run some
# summary statistics.

stargazer(eclipse_data, type='text') #OR
summary(eclipse_data)


# Next, partition the data so it includes a training
# set and a testing set. 


set.seed(123456)
index <- sample(nrow(eclipse_data), nrow(eclipse_data)*.60)
train = eclipse_data[index,] 
test = eclipse_data[-index,]


# Utilize the training data set to estimate a prediction model.
# With this, observe the ROC curve obtained under both the
# training data and the test data, as well as the area under
# the curve. 


set.seed(123456)
logit_model <- glm(curr_res ~ ., family = binomial(link = "logit"), 
             data = train)

summary(logit_model)


# From this, note the significance level of our covariates. The
# majority of these variables are highly significant; the only one
# that does not appear to be significant is component.

# Now, predict with the training data.


pred_prob_train_logit<- predict.glm(logit_model, type="response")

pred_train_logit <- prediction(pred_prob_train_logit, train$curr_res)
perf_train_logit <- performance(pred_train_logit, "tpr", "fpr")


# Calculate the area under the curve.

unlist(slot(performance(pred_train_logit, "auc"), "y.values"))

# Now, use this model to predict with the test data and observe
# the results.

pred_prob_test_logit <- predict.glm(logit_model, newdata=test, type="response")

pred_test_logit <- prediction(pred_prob_test_logit, test$curr_res)
perf_test_logit <- performance(pred_test_logit, "tpr", "fpr")

unlist(slot(performance(pred_test_logit, "auc"), "y.values"))


# The area under the ROC curve is very similar for both predictions
# of the training data set and the testing data set, above .88 for
# each.

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
  perf_logit <- calculate_performance(pred_prob_test_logit, test$curr_res, thresholds[i])
  sensitivity[i] <- perf_logit$sensitivity
  specificity[i] <- perf_logit$specificity
  accuracy[i] <- perf_logit$accuracy
}

best_accuracy <- max(accuracy)
best_sensitivity <- sensitivity[which.max(accuracy)]
best_specificity <- specificity[which.max(accuracy)]


best_threshold <- thresholds[which.max(accuracy)]
best_threshold


plot(perf_test_logit, main = "ROC Curve Logit", colorize = TRUE)
legend("bottomright", legend = paste("Threshold =", round(best_threshold, 2)), col = "black", lty = 1)
abline(a=0,b=1)

# Now, calculate the TPR, TNR, FPR, and FNR. 

class_prediction_logit <- ifelse(pred_prob_test_logit > best_threshold, "positive_class", "negative_class")

y <- table(test$curr_res)
table(class_prediction_logit)


confmat <- table(class_prediction_logit, test$curr_res)
confmat

tpr <- confmat[2,2]/y[[2]]
fpr <- confmat[2,1]/y[[1]]
tnr <- confmat[1,1]/y[[1]]
fnr <- confmat[1,2]/y[[2]]

tpr
fpr
tnr
fnr



# From here, we should utilize LASSO regularization, which will
# force irrelevant variables to be exactly 0, eliminating them
# from the final model. 

set.seed(123456)
x <- model.matrix(curr_res ~., data = train)
y <- train$curr_res

# Use cross-validation to determine the optimal lambda value.

set.seed(123456)
cv.lasso <- cv.glmnet(x,y,alpha=1,family="binomial", type.measure = "auc")
cv.lasso$lambda.min
cv.lasso$lambda.1se

# Generate models utilizing these lambdas:

set.seed(123456)
LASSO_model_min <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)

coef(LASSO_model_min)

set.seed(123456)
LASSO_model_1se <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.1se)

coef(LASSO_model_1se)

# Predict with the training data and calculate the AUC
# for both models.

pred_prob_train_LASSO_min <- predict(LASSO_model_min, x, s = "lambda.min", type = "response")
pred_train_LASSO_min <- prediction(pred_prob_train_LASSO_min, train$curr_res)
perf_train_LASSO_min <- performance (pred_train_LASSO_min, "tpr", "fpr")

unlist(slot(performance(pred_train_LASSO_min, "auc"), "y.values"))


pred_prob_train_LASSO_1se <- predict(LASSO_model_1se, x, s = "lambda.1se", type = "response")
pred_train_LASSO_1se <- prediction(pred_prob_train_LASSO_1se, train$curr_res)
perf_train_LASSO_1se <- performance (pred_train_LASSO_1se, "tpr", "fpr")

unlist(slot(performance(pred_train_LASSO_1se, "auc"), "y.values"))


# Continue this process with the test data. 

x.test <- model.matrix(curr_res ~., data = test)

pred_prob_test_LASSO_min <- predict(LASSO_model_min, newx = x.test, s = "lambda.min", type = "response")
pred_test_LASSO_min <- prediction(pred_prob_test_LASSO_min, test$curr_res)
perf_test_LASSO_min <- performance(pred_test_LASSO_min, "tpr", "fpr")

unlist(slot(performance(pred_test_LASSO_min, "auc"), "y.values"))


pred_prob_test_LASSO_1se <- predict(LASSO_model_1se, newx = x.test, s = "lambda.1se", type = "response")
pred_test_LASSO_1se <- prediction(pred_prob_test_LASSO_1se, test$curr_res)
perf_test_LASSO_1se <- performance(pred_test_LASSO_1se, "tpr", "fpr")

unlist(slot(performance(pred_test_LASSO_1se, "auc"), "y.values"))


# Again, the area under the curve for both training and
# testing is very similar, and the results do not differ
# too significantly from the results under the simple logit
# model. The auc for the LASSO logit model is better than
# what was obtained using the logit model, but only by a 
# slight amount.

# Again, we can determine the optimal threshold that yields
# the greatest accuracy, and calculate the tpr, fpr, tnr, and
# fnr for both LASSO models.

thresholds <- seq(0.01, 0.99, by = 0.01)
sensitivity <- numeric(length(thresholds))
specificity <- numeric(length(thresholds))
accuracy <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  perf_LASSO_min <- calculate_performance(pred_prob_test_LASSO_min, test$curr_res, thresholds[i])
  sensitivity[i] <- perf_LASSO_min$sensitivity
  specificity[i] <- perf_LASSO_min$specificity
  accuracy[i] <- perf_LASSO_min$accuracy
}

best_accuracy <- max(accuracy)
best_sensitivity <- sensitivity[which.max(accuracy)]
best_specificity <- specificity[which.max(accuracy)]


best_threshold <- thresholds[which.max(accuracy)]
best_threshold

plot(perf_test_LASSO_min, main = "ROC Curve LASSO (min)", colorize = TRUE)
legend("bottomright", legend = paste("Threshold =", round(best_threshold, 2)), col = "black", lty = 1)
abline(a=0,b=1)


thresholds <- seq(0.01, 0.99, by = 0.01)
sensitivity <- numeric(length(thresholds))
specificity <- numeric(length(thresholds))
accuracy <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  perf_LASSO_1se <- calculate_performance(pred_prob_test_LASSO_1se, test$curr_res, thresholds[i])
  sensitivity[i] <- perf_LASSO_1se$sensitivity
  specificity[i] <- perf_LASSO_1se$specificity
  accuracy[i] <- perf_LASSO_1se$accuracy
}

best_accuracy <- max(accuracy)
best_sensitivity <- sensitivity[which.max(accuracy)]
best_specificity <- specificity[which.max(accuracy)]


best_threshold <- thresholds[which.max(accuracy)]
best_threshold


plot(perf_test_LASSO_1se, main = "ROC Curve LASSO (1se)", colorize = TRUE)
legend("bottomright", legend = paste("Threshold =", round(best_threshold, 2)), col = "black", lty = 1)
abline(a=0,b=1)


# Calculate the tpr, fpr, tnr, and fnr.

class_prediction_LASSO_min <- ifelse(pred_prob_test_LASSO_min 
                                     > best_threshold, "positive_class", "negative_class")

y <- table(test$curr_res)
table(class_prediction_LASSO_min)


confmat <- table(class_prediction_LASSO_min, test$curr_res)
confmat

tpr <- confmat[2,2]/y[[2]]
fpr <- confmat[2,1]/y[[1]]
tnr <- confmat[1,1]/y[[1]]
fnr <- confmat[1,2]/y[[2]]

tpr
fpr
tnr
fnr


class_prediction_LASSO_1se <- ifelse(pred_prob_test_LASSO_1se
                                     > best_threshold, "positive_class", "negative_class")

table(class_prediction_LASSO_1se)


confmat <- table(class_prediction_LASSO_1se, test$curr_res)
confmat

tpr <- confmat[2,2]/y[[2]]
fpr <- confmat[2,1]/y[[1]]
tnr <- confmat[1,1]/y[[1]]
fnr <- confmat[1,2]/y[[2]]

tpr
fpr
tnr
fnr


# Now, we will move onto bagging and random forest.

set.seed(123456)
bagging_model <- bagging(curr_res ~., train, nbagg=150, coob = TRUE, control=rpart.control(minsplit=2, cp=0))

bagging_model

# Obtain predictions for bagging_model.

pred_prob_train_bagging <- predict(bagging_model, type = "prob")[,2]
pred_train_bagging <- prediction(pred_prob_train_bagging, train$curr_res)
perf_train_bagging <- performance(pred_train_bagging, "tpr", "fpr")

# Calculate the area under the curve.

unlist(slot(performance(pred_train_bagging, "auc"), "y.values"))


# Predict with the test data and calculate the auc. 

pred_prob_test_bagging <- predict(bagging_model, newdata = test, type = "prob")[,2]
pred_test_bagging <- prediction(pred_prob_test_bagging, test$curr_res)
perf_test_bagging <- performance(pred_test_bagging, "tpr", "fpr")

unlist(slot(performance(pred_test_bagging, "auc"), "y.values"))


# Calculate optimal threshold and plot ROC curve.

thresholds <- seq(0.01, 0.99, by = 0.01)
sensitivity <- numeric(length(thresholds))
specificity <- numeric(length(thresholds))
accuracy <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  perf_bagging <- calculate_performance(pred_prob_test_bagging, test$curr_res, thresholds[i])
  sensitivity[i] <- perf_bagging$sensitivity
  specificity[i] <- perf_bagging$specificity
  accuracy[i] <- perf_bagging$accuracy
}

best_accuracy <- max(accuracy)
best_sensitivity <- sensitivity[which.max(accuracy)]
best_specificity <- specificity[which.max(accuracy)]

best_threshold <- thresholds[which.max(accuracy)]
best_threshold


plot(perf_test_bagging, main = "ROC Curve Bagging", colorize = TRUE)
legend("bottomright", legend = paste("Threshold =", round(best_threshold, 2)), col = "black", lty = 1)
abline(a=0,b=1)

# Determine the tpr, fpr, tnr, and fnr.

class_prediction_bagging <- ifelse(pred_prob_test_bagging
                                   > best_threshold, "positive_class", "negative_class")

table(class_prediction_bagging)

confmat <- table(class_prediction_bagging, test$curr_res)
confmat

tpr <- confmat[2,2]/y[[2]]
fpr <- confmat[2,1]/y[[1]]
tnr <- confmat[1,1]/y[[1]]
fnr <- confmat[1,2]/y[[2]]

tpr
fpr
tnr
fnr


# Random Forest.

set.seed(123456)
RF_model <- randomForest(curr_res ~ ., data = train, ntree = 1000, mtry=5)

RF_model
summary(RF_model)


pred_prob_train_RF <- predict(RF_model, type="prob")[,2]
pred_train_RF <- prediction(pred_prob_train_RF, train$curr_res)
perf_train_RF <- performance(pred_train_RF, "tpr", "fpr")

# Calculate the area under the curve.

unlist(slot(performance(pred_train_RF, "auc"), "y.values"))


# Now, use this model to predict with the test data and observe
# the results.

pred_prob_test_RF <- predict(RF_model, newdata=test, type="prob")[,2]
pred_test_RF <- prediction(pred_prob_test_RF, test$curr_res)
perf_test_RF <- performance(pred_test_RF, "tpr", "fpr")

unlist(slot(performance(pred_test_RF, "auc"), "y.values"))

# Again, we can determine the optimal threshold that yields
# the greatest accuracy, plot the ROC curve, and calculate 
# the tpr, fpr, tnr, and fnr.


thresholds <- seq(0.01, 0.99, by = 0.01)
sensitivity <- numeric(length(thresholds))
specificity <- numeric(length(thresholds))
accuracy <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  perf_RF <- calculate_performance(pred_prob_test_RF, test$curr_res, thresholds[i])
  sensitivity[i] <- perf_RF$sensitivity
  specificity[i] <- perf_RF$specificity
  accuracy[i] <- perf_RF$accuracy
}

best_accuracy <- max(accuracy)
best_sensitivity <- sensitivity[which.max(accuracy)]
best_specificity <- specificity[which.max(accuracy)]

best_threshold <- thresholds[which.max(accuracy)]
best_threshold


plot(perf_test_RF, main = "ROC Curve Random Forest", colorize = TRUE)
legend("bottomright", legend = paste("Threshold =", round(best_threshold, 2)), col = "black", lty = 1)
abline(a=0,b=1)

# Determine the tpr, fpr, tnr, and fnr.

class_prediction_RF <- ifelse(pred_prob_test_RF > best_threshold, "positive_class", "negative_class")

table(class_prediction_RF)


confmat <- table(class_prediction_RF, test$curr_res)
confmat

tpr <- confmat[2,2]/y[[2]]
fpr <- confmat[2,1]/y[[1]]
tnr <- confmat[1,1]/y[[1]]
fnr <- confmat[1,2]/y[[2]]

tpr
fpr
tnr
fnr


# Finally, we should utilize boosting to determine if
# we generate a higher area under the curve.

train$curr_res <- as.numeric(as.character(train$curr_res))
test$curr_res <- as.numeric(as.character(test$curr_res))

set.seed(123456)
boost_model <- gbm(curr_res ~ ., distribution = "bernoulli", data = train,
                  n.trees = 10000, shrinkage = .1, cv.folds = 10)

boost_model
summary(boost_model)

# Predict using the training data.

pred_prob_train_boost <- predict(boost_model, type="response")

pred_train_boost <- prediction(pred_prob_train_boost, train$curr_res)
perf_train_boost <- performance(pred_train_boost, "tpr", "fpr")


# Calculate the area under the curve.

unlist(slot(performance(pred_train_boost, "auc"), "y.values"))


# Now, use this model to predict with the test data and observe
# the results.

pred_prob_test_boost <- predict(boost_model, newdata=test, type="response")

pred_test_boost <- prediction(pred_prob_test_boost, test$curr_res)
perf_test_boost <- performance(pred_test_boost, "tpr", "fpr")

unlist(slot(performance(pred_test_boost, "auc"), "y.values"))


# Again, we can determine the optimal threshold that yields
# the greatest accuracy, and calculate the tpr, fpr, tnr, and
# fnr.


thresholds <- seq(0.01, 0.99, by = 0.01)
sensitivity <- numeric(length(thresholds))
specificity <- numeric(length(thresholds))
accuracy <- numeric(length(thresholds))

for (i in seq_along(thresholds)) {
  perf_boost <- calculate_performance(pred_prob_test_boost, test$curr_res, thresholds[i])
  sensitivity[i] <- perf_boost$sensitivity
  specificity[i] <- perf_boost$specificity
  accuracy[i] <- perf_boost$accuracy
}

best_accuracy <- max(accuracy)
best_sensitivity <- sensitivity[which.max(accuracy)]
best_specificity <- specificity[which.max(accuracy)]


best_threshold <- thresholds[which.max(accuracy)]
best_threshold


plot(perf_test_boost, main = "ROC Curve Boosting", colorize = TRUE)
legend("bottomright", legend = paste("Threshold =", round(best_threshold, 2)), col = "black", lty = 1)
abline(a=0,b=1)


# Calculate the tpr, fpr, tnr, and fnr.

class_prediction_boost <- ifelse(pred_prob_test_boost > best_threshold, "positive_class", "negative_class")

table(class_prediction_boost)


confmat <- table(class_prediction_boost, test$curr_res)
confmat

tpr <- confmat[2,2]/y[[2]]
fpr <- confmat[2,1]/y[[1]]
tnr <- confmat[1,1]/y[[1]]
fnr <- confmat[1,2]/y[[2]]

tpr
fpr
tnr
fnr












