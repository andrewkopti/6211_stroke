library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)
library(ISLR)
library(randomForest)

filename <- "healthcare-dataset-stroke-data.csv"

# read csv into R
# define column types
df <- read_csv(
  filename,
  col_types = cols(
    gender = col_factor(levels = c("Male", "Female", "Other")),
    age = col_integer(),
    hypertension = col_logical(),
    heart_disease = col_logical(),
    ever_married = col_factor(levels = c("Yes", "No")),
    work_type = col_factor(levels = c( "children", "Govt_jov", "Never_worked", "Private", "Self-employed")),
    Residence_type = col_factor(levels = c( "Rural", "Urban")),
    avg_glucose_level = col_double(),
    bmi = col_double(),
    smoking_status = col_factor(levels = c("formerly smoked", "never smoked", "smokes", "Unknown")),
    stroke = col_factor()
  )
)

# drop NA values and id column
df <- drop_na(df)
df <- select(df, -id)

# split dataset
set.seed(5)
train_index <- createDataPartition(df$stroke, 
                                  p = 0.7,
                                  list = FALSE, 
                                  times = 1) 
df_train <- df[train_index, ]
df_valid <- df[-train_index, ]

# decision tree model
tree_model <- train(stroke ~., 
                    data = df_train, 
                    method = "rpart", 
                    na.action = na.pass)

tree_model
prp(tree_model$finalModel, type = 2, extra = 100)
prediction <- predict(tree_model, newdata = df_valid, na.action = na.pass)
confusionMatrix(prediction, df_valid$stroke)
tree_probabilities <- predict(tree_model, newdata = df_valid, type = "prob", na.action = na.pass)
tree_roc <- roc(predictor = tree_probabilities$'1', 
                response = df_valid$stroke, 
                levels = levels(df_valid$stroke))
plot(tree_roc)

# random forest model
rf_model <- train(stroke~., 
                    data = df_train, 
                    method = "rf", 
                    metric = "Accuracy",
                    ntree = 50)
print(rf_model)
tune_grid <- expand.grid(.mtry = c(1:17))


rf_mtry <- train(Private~., 
                 data = df.train, 
                 method = "rf", 
                 metric = "Accuracy",
                 tuneGrid = tuneGrid, 
                 importance = TRUE, 
                 ntree = 50)
print(rf_mtry)
plot(rf_mtry)

varImp(rf_mtry)
plot(varImp(rf_mtry))

