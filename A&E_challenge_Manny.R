# Code to explore A&E data and predict whether patients have been admitted.

# Install relevant packages
# install.packages('caret','tidyverse')
# install.packages("ggcorrplot")
# install.packages('caTools', repos = "http://cran.us.r-project.org")
# install.packages("httpgd", repos = "http://cran.us.r-project.org")
# install.packages("languageserver", repos = "http://cran.us.r-project.org")
# install.packages('skimr', repos = "http://cran.us.r-project.org")
# install.packages("corrplot", repos = "http://cran.us.r-project.org")
# install.packages("fastDummies", repos = "http://cran.us.r-project.org")
# install.packages("lubridate", repos = "http://cran.us.r-project.org")
# install.packages("randomForest", repos = "http://cran.us.r-project.org")
# install.packages("tictoc", repos = "http://cran.us.r-project.org")
# install.packages("gbm")

# Load required packages
library(tidyverse)
library(caTools)
library(caret)
library(dplyr)
library(skimr) # used for exploratory data analysis
library(corrplot)
library(fastDummies)
library(lubridate)
library(readxl)
library(gbm)
library(xgboost)
library(data.table)
library(tictoc) # used to measure how long things take to run


#A. Data collection and importing
# setting working directory
setwd("C:\\Users\\eo375\\Downloads\\bip-ae-technical-challenge")
train_data <- read.csv("training_set.csv")
test_data <- read.csv("test_set.csv")
skinny_set <- read.csv("skinny_unanswered_set.csv")
answer_sheet <- read.csv("example_answer_sheet.csv")
data_dict <- read_excel("Data_Dictionary.xlsx")


#B. Exploratory Data Analysis (EDA)
# dimensions of dataset
dim(train_data)

# To display the first & last five rows of our data
head(train_data)
tail(train_data)

# Summary statistics
glimpse(train_data)

#Task 1. Visualize variable distributions
# Split variables into numerical or categorical
num_vars <- 
  train_data %>% select(where(is.numeric), -Admitted_Flag)

cat_vars <- 
  train_data %>% select(!where(is.numeric))

glimpse(num_vars)
glimpse(cat_vars)


#Task 2. Create validation data set
# setting seed to generate a reproducible random sampling
set.seed(100)
# dividing the data set into an 80:20 ratio
spl <- createDataPartition(train_data$Admitted_Flag, p = 0.8, list = FALSE)
# selecting part of data set which belongs to the 80% for training
train <- train_data[spl, ]
# selecting part of data set which belongs to the 20% for validation
val <- train_data[-spl, ]

# Store X and Y for later use.
x <- train %>% select(-Admitted_Flag)
y <- train$Admitted_Flag

# overview of training data
skim(train)


#C. Data Pre-processing
#Task 1. Deal with missing values in the training set
sum(is.na(train))

# Remove columns with excessive number of NA values
thresh <- 0.5    # Define threshold for NA values

#Identify columns with more than 50% NA
colnames(train[which(colMeans(is.na(train)) > thresh)])

# Set any null values in the 'Sex' column to 0
train$Sex[is.na(train$Sex)] <- 0
val$Sex[is.na(val$Sex)] <- 0

# Set any null values in the 'Provider_Patient_Distance_Miles' 
# column of train to the mean of the column
train$Provider_Patient_Distance_Miles[is.na(train$Provider_Patient_Distance_Miles)] <- mean(train$Provider_Patient_Distance_Miles, na.rm = TRUE)
val$Provider_Patient_Distance_Miles[is.na(val$Provider_Patient_Distance_Miles)] <- mean(val$Provider_Patient_Distance_Miles, na.rm = TRUE)

# Set any null values in the 'IMD_Decile_From_LSOA' column to 5
train$IMD_Decile_From_LSOA[is.na(train$IMD_Decile_From_LSOA)] <- 5
val$IMD_Decile_From_LSOA[is.na(val$IMD_Decile_From_LSOA)] <- 5

# # Set any null values of 'ICD10_Chapter_Code' to 'OTHER''
train$ICD10_Chapter_Code[is.na(train$ICD10_Chapter_Code)] <- "OTHER"
val$ICD10_Chapter_Code[is.na(val$ICD10_Chapter_Code)] <- "OTHER"

# Replace 'NA' in "EA_HRG" with the value "Nothing"
train$AE_HRG[is.na(train$AE_HRG)] <- "Nothing"
val$AE_HRG[is.na(val$AE_HRG)] <- "Nothing"

# # Set any null values of 'Treatment_Function_Code' to 'OTHER''
train$Treatment_Function_Code[is.na(train$Treatment_Function_Code)] <- "OTHER"
val$Treatment_Function_Code[is.na(val$Treatment_Function_Code)] <- "OTHER"

# Set any null values in the 'IMD_Decile_From_LSOA' column to 5
train$IMD_Decile_From_LSOA[is.na(train$IMD_Decile_From_LSOA)] <- 5
val$IMD_Decile_From_LSOA[is.na(val$IMD_Decile_From_LSOA)] <- 5

# Set any null values of 'Length_Of_Stay_Days' to a random integer between 1-45
train$Length_Of_Stay_Days[is.na(train$Length_Of_Stay_Days)] <- sample(1:45, 
                    sum(is.na(train$Length_Of_Stay_Days)), replace=TRUE)
val$Length_Of_Stay_Days[is.na(val$Length_Of_Stay_Days)] <- sample(1:45,
                   sum(is.na(val$Length_Of_Stay_Days)), replace=TRUE)

# Drop 'AE_Arrive_HourOfDay' column
train <- train %>% select(-AE_Arrive_HourOfDay)
val <- val %>% select(-AE_Arrive_HourOfDay)

#Task 2. Create One-Hot Encoding(dummy variables) for categorical variables
train <- dummy_cols(train, select_columns = "AE_HRG",
                    remove_first_dummy = TRUE, remove_selected_columns = TRUE)
val <- dummy_cols(val, select_columns = "AE_HRG",
                  remove_first_dummy = TRUE, remove_selected_columns = TRUE)

#Task 3. Normalize data fields
# Convert 'AE_Arrive_Date' to datetime format
train$AE_Arrive_Date <- as.POSIXct(train$AE_Arrive_Date)
val$AE_Arrive_Date <- as.POSIXct(val$AE_Arrive_Date)

# Extract date components
train <- train %>%
  mutate(Arrival_Year = year(AE_Arrive_Date),
         Arrival_Month = month(AE_Arrive_Date),
         Arrival_Day = day(AE_Arrive_Date))

val <- val %>%
  mutate(Arrival_Year = year(AE_Arrive_Date),
         Arrival_Month = month(AE_Arrive_Date),
         Arrival_Day = day(AE_Arrive_Date))

#Task 4. Re-analyse the new training set
# Summary Statistics
sample(train)

# Check for for NA values in the predictors
colSums(is.na(train[, -which(names(train) == "Admitted_Flag")]))

## Explore correlation between variables
# Correlation matrix for numeric columns
num_col <- train %>% select(where(is.numeric))
# Display the correlation matrix as a heat map
cor_matrix <- cor(num_col, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color")

# Flag any columns that have zero or near-zero variance
nzv <- nearZeroVar(train, saveMetrics= T)
nzv


#D. Model training and Evaluation
# Run a basic model
# convert num_col to dataframe
num_col <- data.frame(num_col)

# convert Admitted_Flag column to a 2 level factor as outcome column.
num_col$Admitted_Flag <- as.factor(num_col$Admitted_Flag)
val$Admitted_Flag <- as.factor(val$Admitted_Flag)

# Sample with varying size of the data.
sample_size <- nrow(num_col) * 0.075
train_subset <- num_col %>% sample_n(sample_size)

# Run algorithms using cross validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"

# Train model using different algorithms
# C5.0
set.seed(seed)
fit.c50 <- train(Admitted_Flag~., data=train_subset, method="C5.0",
                 metric=metric, trControl=control)
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm <- train(Admitted_Flag~., data=train_subset, method="gbm",
                 metric=metric, trControl=control, verbose=FALSE)
# Extreme Gradient Boosting
set.seed(seed)
fit.xgb <- train(Admitted_Flag~., data=train_subset, method="xgbTree",
                 metric=metric, trControl=control, verbose=FALSE, verbosity=0)
# Random Forest
set.seed(seed)
fit.rf <- train(Admitted_Flag~., data=train_subset, method="rf",
                metric=metric, trControl=control)
# SVM - Support Vector Machine
set.seed(seed)
fit.svm <- train(Admitted_Flag~., data=train_subset, method="svmRadial",
                 metric=metric, trControl=control)
# summarize accuracy of models
results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm,xgb=fit.xgb,
                          rf=fit.rf, svm=fit.svm))
# summarize results
summary(results)
# compare accuracy of models
dotplot(results)
# summarize Best Model
print(fit.gbm)

# estimate variable importance
importance <- varImp(fit.rf, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# Estimate skill of model on the validation dataset
predictions <- predict(fit.gbm, val)
confusionMatrix(as.factor(predictions), val$Admitted_Flag)


#F. Make predictions using test data and the most accurate model.
# Task 1. Prepare test_data and make predictions
# Pre-processing
test_data$Admitted_Flag <- NA
test <- test_data
# Deal with missing values in the test set
sum(is.na(test))
# Identify columns with more than 50% NA
colnames(test[which(colMeans(is.na(test)) > thresh)])
# Set any null values in the 'Sex' column to 0
test$Sex[is.na(test$Sex)] <- 0

# Set any null values in the 'Provider_Patient_Distance_Miles' 
# column of train to the mean of the column
test$Provider_Patient_Distance_Miles[is.na(test$Provider_Patient_Distance_Miles)] <- mean(test$Provider_Patient_Distance_Miles, na.rm = TRUE)
# Set any null values in the 'IMD_Decile_From_LSOA' column to 5
test$IMD_Decile_From_LSOA[is.na(test$IMD_Decile_From_LSOA)] <- 5

# # Set any null values of 'ICD10_Chapter_Code' to 'OTHER''
test$ICD10_Chapter_Code[is.na(test$ICD10_Chapter_Code)] <- "OTHER"
# Replace 'NA' in "EA_HRG" with the value "Nothing"
test$AE_HRG[is.na(test$AE_HRG)] <- "Nothing"
# # Set any null values of 'Treatment_Function_Code' to 'OTHER''
test$Treatment_Function_Code[is.na(test$Treatment_Function_Code)] <- "OTHER"

# Set any null values in the 'IMD_Decile_From_LSOA' column to 5
test$IMD_Decile_From_LSOA[is.na(test$IMD_Decile_From_LSOA)] <- 5
# Set any null values of 'Length_Of_Stay_Days' to a random integer between 1-45
test$Length_Of_Stay_Days[is.na(test$Length_Of_Stay_Days)] <- sample(1:45, 
                      sum(is.na(test$Length_Of_Stay_Days)), replace=TRUE)
# Drop 'AE_Arrive_HourOfDay' column
test <- test %>% select(-AE_Arrive_HourOfDay)


#Task 2. Create One-Hot Encoding(dummy variables) and 
# ordinal encoding for categorical variables
test <- dummy_cols(test, select_columns = "AE_HRG",
                    remove_first_dummy = TRUE, remove_selected_columns = TRUE)

#Task 3. Normalize data fields
# Convert 'AE_Arrive_Date' to datetime format
test$AE_Arrive_Date <- as.POSIXct(test$AE_Arrive_Date)

# Extract date components
test <- test %>%
  mutate(Arrival_Year = year(AE_Arrive_Date),
         Arrival_Month = month(AE_Arrive_Date),
         Arrival_Day = day(AE_Arrive_Date))

str(test)

# Predict on test_data
output <- predict(fit.gbm, test)
summary(output)

# Create dataframe from predictions
Record_ID <- test$Record_ID
Admitted_Flag <- output
final_answer_sheet <- data.frame(Record_ID, Admitted_Flag)
str(final_answer_sheet)

# read dataframe to csv file
fwrite(final_answer_sheet, "C:\\Users\\eo375\\Downloads\\bip-ae-technical-challenge\\final_answer_sheet.csv")