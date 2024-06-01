########################
###  Welcome back to ###
##  Machine Learning  ##
########################

library(caret)
library(glmnet)
library(randomForest)
library(xgboost)
library(xgboostExplainer)
library(fastDummies)

## copy data with dummy columns in place of categories
stroke2 <- dummy_cols(stroke, remove_selected_columns = TRUE)

## removing the "other" gender
stroke <- stroke[stroke$gender != "Other", ]

## partition data
set.seed(1)
n_obs <- nrow(stroke)
train_index <- sample(1 : n_obs, 0.7 * n_obs)
train <- stroke[train_index, ]
test <- stroke[-train_index, ]
train2 <- stroke2[train_index, ]
test2 <- stroke2[-train_index, ]

## logistic model
log_train <- train
log_train$hypertension <- as.factor(log_train$hypertension)
log_train$heart_disease <- as.factor(log_train$heart_disease)
log_train$stroke <- as.factor(log_train$stroke)

log_test <- test
log_test$hypertension <- as.factor(log_test$hypertension)
log_test$heart_disease <- as.factor(log_test$heart_disease)
log_test$stroke <- as.factor(log_test$stroke)

log_model <- glm(stroke ~., log_train, family = "binomial"(link = "logit"))
summary(log_model)

log_preds <- predict(log_model, newdata = log_test, type = "response")

mat1 <- confusionMatrix(factor(ifelse(log_preds > 0.1, "1", "0")), as.factor(test$stroke), positive = "1")
mat1

## lasso
lasso <- scale(stroke2[, c(2:6, 8:23)])

l_seq <- 10^seq(4, -4, by = -.1)
lasso_fit <- cv.glmnet(lasso, as.factor(stroke2$stroke), alpha = 1, lambda = l_seq, 
                       family = "binomial", nfolds = 10)
lasso_fit$lambda.min

lasso_best <- glmnet(lasso, as.factor(stroke2$stroke), alpha = 1, 
                     lambda = lasso_fit$lambda.min, family = "binomial")

coef(lasso_best)

## random forest
set.seed(1)
rf_model <- randomForest(as.factor(stroke)  ~.,
                         data = train[, -1],
                         ntree = 500,
                         nodesize = 1,
                         mtry = 10)

rf_preds <- predict(rf_model, test, type = "prob")

rf_pred_class <- rep("0", nrow(rf_preds))
rf_pred_class[rf_preds[,2] >= 0.1] <- "1"

t1 <- table(rf_pred_class, test$stroke)
confusionMatrix(t1, positive = "1")

## xgboost
dtrain <- xgb.DMatrix(data = as.matrix(train2[, c(2:6, 8:23)]), label = as.numeric(train2$stroke))
dtest <- xgb.DMatrix(data = as.matrix(test2[, c(2:6, 8:23)]), label = as.numeric(test2$stroke))

set.seed(1)
boost <- xgboost(data = dtrain,
                 nrounds = 200,
                 eta = 0.05,
                 verbose = 1,
                 print_every_n = 20,
                 objective = "binary:logistic",
                 eval_metric = "auc",
                 eval_metric = "error")

boost_preds <- predict(boost, dtest)

pred_dat <- cbind.data.frame(boost_preds, test2$stroke)

boost_pred_class <- rep("0", length(boost_preds))
boost_pred_class[boost_preds >= 0.04] <- "1"

t2 <- table(boost_pred_class, test2$stroke)
confusionMatrix(t2, positive = "1")

imp_mat <- xgb.importance(model = boost)
xgb.plot.importance(imp_mat, top_n = 5)

roc1 <- roc(log_test$stroke, log_preds)
roc2 <- roc(test$stroke, rf_preds[, 2])
roc3 <- roc(test2$stroke, boost_preds)

plot.roc(roc1, print.auc = TRUE, col = "red", print.auc.col = "red")
plot.roc(roc2, print.auc = TRUE, print.auc.x = 0, print.auc.y = 0.6, col = "blue", print.auc.col = "blue", add = TRUE)
plot.roc(roc3, print.auc = TRUE, print.auc.x = 0, print.auc.y = 0.8, col = "darkgreen", print.auc.col = "darkgreen", add = TRUE)
