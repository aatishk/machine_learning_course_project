# read the train and test data
train_data <- read.csv("pml-training.csv", na.strings = c("", "NA", "#DIV/0!"))
test_data <- read.csv("pml-testing.csv", na.strings = c("", "NA", "#DIV/0!"))

# number of columns in raw training data set
ncol(train_data)

# caret package for nearZeroVar function
require(caret)

# remove first column (it's a row count)
train_data <- train_data[-1]

# identify predictors with near zero variance
near_zero_variance_preds <- nearZeroVar(train_data, saveMetrics = TRUE)

# remove predictors with near zero variance from training dataset
train_data <- train_data[, !near_zero_variance_preds$nzv]

# identify predictors with 75% missing values
missing_value_preds <- sapply(colnames(train_data), 
                              function(x) if(sum(is.na(train_data[, x])) > 0.75*nrow(train_data))
                                            return(TRUE)
                                          else
                                            return(FALSE)
                              )

# remove predictors with 75% missing values from training dataset
train_data <- train_data[, !missing_value_preds]

# number of columns in cleaned-up training data set
ncol(train_data)

# fit random forests model
# 10-fold cross validation to predict classe with all the predictors left after data cleaning

# randomForest package 
require(randomForest)

# set seed for reproducibility
set.seed(123456)

# start the model fitting
model.rf <- train(classe ~ ., method = "rf", data = train_data, importance = TRUE, 
                  trControl = trainControl(method = "cv", number = 10)
                  )

# print details of the fitted model
summary(model.rf)

# plot the different predictors' contributions to fitted model
plot(varImp(model.rf))

# print details of performance of fitted model
model_fit$finalModel

# do the prediction on the test data set
predicted <- predict(model_fit, newdata=test_data)

# store the predicted values in separate files to be uploaded
# function to write output file
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("./prediction/problem_id_", i, ".txt")
    write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, col.names = FALSE)
  }
}

# call function for each of the predicted values
pml_write_files(predicted)