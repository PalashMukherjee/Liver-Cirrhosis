#setting the working directory
getwd()
setwd("C:\\Users\\Dell\\Desktop\\balsingh project (1)\\Classification Project")
library(readxl)


#importing excel file
df1 <- read_excel("data.xlsx")
dim(df1)
table(df1$Classes)
#View(df1)


#checking the null values across all the columns of df1 and df2
for(i in colnames(df1)){
  print(sum(is.na(df1[,i])))
}
str(df1)

## Dexcriptive statistics
summary(df1)
#plotting
library(ggplot2)
library(GGally)
# Use a colorful palette
my_palette <- c("#FF5733", "#FFBD33", "#33FF57", "#337BFF", "#E033FF")


# Creating pair plot
# Convert 'Classes' column to factor
df1$Classes <- as.factor(df1$Classes)
# Create pair plot with custom color palette
pair_plot <- ggpairs(df1, aes(colour = Classes)) +
  theme_bw() +
  scale_color_manual(values = my_palette)
pair_plot


# Create the training and testing sets using createDataPartition()
train_proportion <- 0.8
set.seed(300)
num_rows <- train_proportion*dim(df1)[1]
random_indices <- sample(nrow(df1),num_rows)
train_dataset <- df1[random_indices,]
train_dataset
test_dataset <- df1[-random_indices,]
test_dataset_X <- test_dataset[,-11]
test_dataset_Y <- test_dataset[,11]
dim(train_dataset)
dim(test_dataset)
test_dataset_X
test_dataset_Y




## Logistic regression ------------------------

#install.packages('Metrics')
library(Metrics)
# Fit logistic regression model with weights
model1 <- glm(Classes ~ ., data =train_dataset, family = binomial)

# Print the model summary
summary(model1)

# Check for multicollinearity
library(car)
vif(model1)

## feature selection
library(MASS)
# Perform likelihood-based feature selection using stepAIC
model1 <- stepAIC(model1, direction = "both")
model1
summary(model1)
vif(model1)


## test data accuracy --------------
predictions <- predict(model1,newdata = test_dataset_X,type="response")
predictions
binary_predictions <- ifelse(predictions >= 0.5, 1, 0)
binary_predictions
# Create the confusion matrix
table1<- table(binary_predictions,test_dataset_Y$Classes)
accuracy_value11 <- sum(diag(table1)) / sum(table1)
accuracy_value11
# Calculate log loss for logistic regression model
log_loss <- logLoss(predict(model1, newdata = test_dataset_X), as.numeric(test_dataset_Y$Classes) - 1)
log_loss

## training data accuracy-----------------
train_predictions <- predict(model1,newdata = train_dataset[,-11])
binary_predictions_train <- ifelse(train_predictions >= 0.5, 1, 0)
binary_predictions_train
# Create the confusion matrix
table<- table(binary_predictions_train,train_dataset$Classes)
# Calculate evaluation metrics based on the confusion matrix
train12 <- sum(diag(table)) / sum(table)
train12
df=data.frame(Training_Accuracy=train12,Test_Accuracy=accuracy_value11)
df

### PRECISION MEASURE , RECALL MEASURE
library(caret)
library(MLmetrics)
# Create a confusion matrix
cm <- confusionMatrix(factor(binary_predictions), factor(test_dataset_Y$Classes))
cm

# Calculate precision, recall, and F1-score
precision <- Precision(binary_predictions,test_dataset_Y$Classes)
precision
recall <- Recall(binary_predictions,test_dataset_Y$Classes)
recall
f1_score <- F1_Score(binary_predictions,test_dataset_Y$Classes)
f1_score
df=data.frame(precision,recall,f1_score)
df


## FITIING SVM MODEL-------------

library(e1071)
svm_model <- svm(Classes ~ ., data = train_dataset, kernel = 'radial', cost = 0.01, gamma = 0.1, type = "C-classification", scale = TRUE)
svm_model
summary(svm_model)

# Acuuracy for test data
predictions <- predict(svm_model,newdata = test_dataset_X)
predictions
# Create the confusion matrix
conf_matrix <- table(predictions,test_dataset_Y$Classes)
# Calculate evaluation metrics based on the confusion matrix
accuracy_valu <- sum(diag(conf_matrix)) / sum(conf_matrix)
# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf_matrix)
print(paste("test data Accuracy:", accuracy_valu))

## train data accuracy
train_predictions <- predict(svm_model,newdata = train_dataset[,-11],type="class")
train_accuracy<- table(train_predictions, train_dataset$Classes)
trai <- sum(diag(train_accuracy)) / sum(train_accuracy)
print(paste("train dataset accuracy",train22))
df=data.frame(Training_Accuracy=trai,Test_Accuracy=accuracy_value21)
df



## tunning of tthe parameter
library(e1071)
# Perform the tuning
train_dataset$Classes <- factor(train_dataset$Classes)
tune.out <- tune(svm, Classes ~ ., data = train_dataset, type = "C-classification", kernel = "radial", scale = TRUE, ranges = list(cost = c(0.1, 1, 10, 100), gamma = c(0.1, 1, 10, 100)))

# Summarize the tuning results
summary(tune.out)

# Get the best model
best_model <- tune.out$best.model
summary(best_model)

# Extract detailed performance results
perf_results <-tune.out$performances
perf_results
# Predict classes on the test dataset
predictions=predict(tune.out$best.model,test_dataset_X)
predictions
## test data accuracy
train_accuracy<- table(predictions,test_dataset_Y$Classes)
accuracy_value00 <- sum(diag(train_accuracy)) / sum(train_accuracy)
print(paste("test dataset accuracy : ",accuracy_value00))

## train data accuracy
train_predictions <- predict(tune.out$best.model,,newdata = train_dataset[,-11])
train_accuracy<- table(train_predictions,train_dataset$Classes)
train22 <- sum(diag(train_accuracy)) / sum(train_accuracy)
print(paste("train dataset accuracy : ",accuracy_value1))
print("confusion matrix for train")
print(train_accuracy)
df=data.frame(Training_Accuracy=train22,Test_Accuracy=accuracy_value00)
df

# Create a confusion matrix
library("caret")
library(MLmetrics)
conf_matrix <- confusionMatrix(predictions,as.factor(test_dataset_Y$Classes))
# Print confusion matrix
print(conf_matrix)

# Calculate precision, recall, and F1-score
precision <- Precision(predictions,test_dataset_Y$Classes)
precision
recall <- Recall(predictions,test_dataset_Y$Classes)
recall
f1_score <- F1_Score(predictions,test_dataset_Y$Classes)
f1_score
df=data.frame(precision,recall,f1_score)
df





## decision trees-----------------------------------
library(tree)
library(rpart)

train_dataset$Classes <- as.factor(train_dataset$Classes)
fit2 <- tree(factor(Classes) ~ ., data = train_dataset)
fit2
summary(fit2)

# Plot the tree
plot(fit2)
text(fit2, pretty = 0)

# teat data accuracy
tree.pred <- predict(fit2,test_dataset_X,type = "class")
tree.pred
# Create the confusion matrix
conf<- table(tree.pred,test_dataset_Y$Classes)
conf
# Calculate evaluation metrics based on the confusion matrix
accuracyvalue31 <- sum(diag(conf)) / sum(conf)
accuracyvalue31
# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf)
print(paste("Accuracy:", accuracyvalue31))


## training data accuracy
train_predictions1 <- predict(fit2,newdata = train_dataset[,-11],type = "class")
conf_matrix <- table(train_predictions1, train_dataset$Classes)
# Calculate accuracy
train2 <- sum(diag(conf_matrix)) / sum(conf_matrix)
train32
df=data.frame(Training_Accuracy=train2,Test_Accuracy=accuracyvalue31)
df


# tuning decision tree for cost
library(rpart)
library(rpart.plot)
# Define the cross-validation function
cv_error <- function(cp, train_data, test_data) {
  tree_model <- rpart(Classes ~ ., data = train_dataset, cp = cp)
  predictions <- predict(tree_model, test_dataset, type = "class")
  error_rate <- mean(predictions !=test_dataset$Classes)
  return(error_rate)
}

# Specify the values of cp to try
cp_values <- seq(0.01, 0.5, by = 0.01)
train_dataset

# Perform cross-validation to find the best cp value
cv_results <- sapply(cp_values, cv_error, train_data = train_dataset, test_data = test_dataset)

# Find the cp value with the lowest error rate
best_cp <- cp_values[which.min(cv_results)]

# Build the final pruned tree using the best cp value
final_tree <- rpart(Classes ~ ., data = train_dataset, cp = best_cp)
pruned_tree <- prune(final_tree, cp = best_cp)
pruned_tree

# Plot the pruned tree
rpart.plot(pruned_tree, type = 0, extra = 101)

# test data accuracy
tree.pred <- predict(pruned_tree,test_dataset_X,type = "class")
tree.pred
# Create the confusion matrix
conf<- table(tree.pred,test_dataset_Y$Classes)
conf
# Calculate evaluation metrics based on the confusion matrix
accuracy_value31 <- sum(diag(conf)) / sum(conf)
accuracy_value31
# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf)
print(paste("Accuracy:", accuracy_value31))

## training data accuracy
train_predictions <- predict(pruned_tree,newdata = train_dataset[,-11],type="class")
conf_matrix <- table(train_predictions, train_dataset$Classes)
# Calculate accuracy
train32 <- sum(diag(conf_matrix)) / sum(conf_matrix)
train32
df=data.frame(Training_Accuracy=train32,Test_Accuracy=accuracy_value31)
df

## PRECISON AND RECALL FOR descion tree
library(caret)
library(MLmetrics)

# Create a confusion matrix
cm <- confusionMatrix(factor(tree.pred), factor(test_dataset_Y$Classes))
cm

# Calculate precision, recall, and F1-score
precision <- Precision(tree.pred,test_dataset_Y$Classes)
precision
recall <- Recall(tree.pred,test_dataset_Y$Classes)
recall
f1_score <- F1_Score(tree.pred,test_dataset_Y$Classes)
f1_score
df=data.frame(precision,recall,f1_score)
df






## random forest
##install.packages("randomForest")
library(randomForest)

# Assuming 'Classes' is numeric or character
train_dataset$Classes <- as.factor(train_dataset$Classes)
model=randomForest(train_dataset$Classes~.,data=train_dataset,ntree=10,method="classification")
model

# Test data accuracy

prediction <- predict(model,newdata =test_dataset_X,type="class")
prediction
# Create the confusion matrix
conf1<- table(prediction, test_dataset_Y$Classes)
conf1
# Calculate evaluation metrics based on the confusion matrix
accuracy_value5 <- sum(diag(conf1)) / sum(conf1)
accuracy_value5
# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf1)
print(paste("Accuracy:", accuracy_value5))


## training data accuracy
train_predictions <- predict(model,newdata = train_dataset[,-11],typr="class")
train5 <- table(train_predictions, train_dataset$Classes)
train5
# Calculate evaluation metrics based on the confusion matrix
tr <- sum(diag(train5)) / sum(train5)
tr
print(paste("train dataset accuracy",tr))
df=data.frame(Training_Accuracy=tr,Test_Accuracy=accuracy_value5)
df


## tunning in random forest

library(randomForest)
library(caret)

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Define the tuning grid
param_grid <- expand.grid(mtry = c(2, 3, 4, 5, 6))

# Tune the Random Forest model
rf_tune <- train(
  Classes ~ .,
  data = train_dataset,
  method = "rf",
  tuneGrid = param_grid,
  trControl = ctrl
)

# Print the tuned model
print(rf_tune)

# Get the best model
best_rf_model <- rf_tune$finalModel
best_rf_model
# Fit the best model to the full training data with reduced number of trees
model <- randomForest(Classes ~ ., data = train_dataset, mtry = best_rf_model$mtry, ntree = 100)

# Print the final model
print(model)

# train data accuracy

# Make predictions on the test dataset
prediction <- predict(model, newdata = test_dataset_X)
# Create the confusion matrix
conf1 <- table(prediction, test_dataset_Y$Classes)
# Calculate evaluation metrics based on the confusion matrix
accuracy_value51 <- sum(diag(conf1)) / sum(conf1)
# Print the confusion matrix and evaluation metrics
print("Confusion Matrix:")
print(conf1)
print(paste("Accuracy:", accuracy_value51))

## training data accuracy
train_predictions <- predict(model, newdata = train_dataset[, -11])
train52 <- table(train_predictions, train_dataset$Classes)
train00=sum(diag(train52)) / sum(train52)
print(paste("Train dataset accuracy", train00))
df=data.frame(Training_Accuracy=train51,Test_Accuracy=accuracy_value52)
df
# Create a confusion matrix
cm <- confusionMatrix(prediction,as.factor(test_dataset_Y$Classes))
cm

# Calculate precision, recall, and F1-score
precision <- Precision(prediction, test_dataset_Y$Classes)
recall <- Recall(prediction, test_dataset_Y$Classes)
f1_score <- F1_Score(prediction, test_dataset_Y$Classes)

# Create a data frame with the metrics
df <- data.frame(precision, recall, f1_score)
print(df)



## combining the accuracy of data
d=c("logistic regression"," SVM","Descion tree","random forest")
d1=c(accuracy_value11,accuracy_value21,accuracy_value31,accuracy_value51)
d2=c(train12,train22,train32,train52)
df=data.frame(model=d,test_accuracy=d1,train_accuracy=d2)
df