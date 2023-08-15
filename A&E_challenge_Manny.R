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

# Load required packages
library(tidyverse)
library(caTools)
library(caret)
library(dplyr)
library(skimr) # used for exploratory data analysis
library(corrplot)
library(fastDummies)
library(lubridate)
library(tictoc) # used to measure how long things take to run

#A. Import all relevant files & read into dataframes
# setting working directory
setwd("bip-ae-technical-challenge")
train_data <- read.csv("training_set.csv")
test_data <- read.csv("test_set.csv")


#Task 1. Explore the datasets
# Summary statistics
glimpse(train_data)
#view(train_data)

# Visualize variable distributions
# Split variables into numerical or categorical
num_vars <- 
  train_data %>% select(where(is.numeric), -Admitted_Flag)

cat_vars <- 
  train_data %>% select(!where(is.numeric))

glimpse(num_vars)
glimpse(cat_vars)

#for (i in 1:ncol(num_vars)) {
#  hist(num_vars[[i]],
#       main = paste("Histogram of", colnames(num_vars)[i]),
#       col = "blue", ylim = c(0, 80000))
#}


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


# Task 3. Pre-processing
# Count missing values in the training set
sum(is.na(train))

# Remove columns with excessive number of NA values
thresh <- 0.5    # Define threshold for NA values

#Identify columns with more than 50% NA
colnames(train[which(colMeans(is.na(train)) > thresh)])

# Set any null values in the 'Sex' column of train to 0
train$Sex[is.na(train$Sex)] <- 0

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

# create One-Hot Encoding(dummy variables) and 
# ordinal encoding for categorical variables
train <- dummy_cols(train, select_columns = "AE_HRG",
                    remove_first_dummy = TRUE, remove_selected_columns = TRUE)
val <- dummy_cols(val, select_columns = "AE_HRG",
                  remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Normalize age & datetime fields
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
#cor_matrix <- cor(num_col, use = "pairwise.complete.obs")
#corrplot(cor_matrix, method = "color")

# Flag any columns that have zero or near-zero variance
nzv <- nearZeroVar(train, saveMetrics= T)
nzv


#E. Evaluate ML algorithms
# Run a basic model
# convert train to dataframe
num_col <- data.frame(num_col)
# convert Admitted_Flag column to a 2 level factor as outcome column.
num_col$Admitted_Flag <- as.factor(num_col$Admitted_Flag)

# Build a basic model
# Sample a quarter of the data. Gonna test with full data 
# to see if model accuracy improves.
sample_size <- nrow(num_col) * 0.05
train_subset <- num_col %>% sample_n(sample_size)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(Admitted_Flag~., data=train_subset, method="lda", metric=metric, trControl=control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Admitted_Flag~., data=train_subset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Admitted_Flag~., data=train_subset, method="knn", metric=metric, trControl=control)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Admitted_Flag~., data=train_subset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Admitted_Flag~., data=train_subset, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.svm)


#F. Make predictions using test data and the most accurate model.


#G. Create ensemble predictions for improved accuracy (Stretch goal)
# - Bagging
# - Boosting
# - Stacking