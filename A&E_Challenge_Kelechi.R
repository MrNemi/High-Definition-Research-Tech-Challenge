#Code to explore A&E data and predict whether patients have been admitted.

# Install relevant packages
#install.packages('caret','tidyverse','caTools')
# install.packages("ggcorrplot", "caTools")
# install.packages('caTools', repos = "http://cran.us.r-project.org")
# install.packages("httpgd", repos = "http://cran.us.r-project.org")
# install.packages("languageserver", repos = "http://cran.us.r-project.org")
# install.packages('skimr', repos = "http://cran.us.r-project.org")
# install.packages("corrplot", repos = "http://cran.us.r-project.org")
# install.packages("fastDummies", repos = "http://cran.us.r-project.org")
# install.packages("lubridate", repos = "http://cran.us.r-project.org")

# Load required packages
library(tidyverse)
library(caTools)
library(caret)
library(dplyr)
library(skimr)
library(corrplot)
library(fastDummies)
library(lubridate)


#A. Import all relevant files & read into dataframes
#download.file(url = "",destfile = "*.csv")
file_path <- "bip-ae-technical-challenge/training_set.csv"
train_data <- read.csv(file_path)
test_path <- "bip-ae-technical-challenge/test_set.csv"
test_data <- read.csv(test_path)


#Task 1. Explore the datasets
# Checking dimensions. Shows number of columns/features. Makes sense for test to gave one less.
dim(train_data)
dim(test_data)

# Summary statistics (str = structure)
str(train_data)
# Name / type / first 10 values
summary(train_data)

# Visualize variable distributions
# Histogram for numeric variables
num_col <- train_data %>% select(where(is.numeric))
str(num_col)

for (i in 1:ncol(num_col)) {
  hist(num_col[[i]],
        main = paste("Histogram of", colnames(num_col)[i]),
        col = "blue", ylim = c(0, 80000))
}

# Bar plots for categorical variables
var_col <- train_data %>% select(!where(is.numeric))
str(var_col)

#1. Patient Admittance
table(train_data$Admitted_Flag)
ggplot(train_data, aes(x = Admitted_Flag)) +
  geom_histogram(bins=30, fill = "magenta")+
  labs(title = "Distribution of Patient Admittance",
       x = "Admitted_Flag",
       y = "Number of patients")


## Correlation between variables

# Correlation matrix for numeric columns
num_col <- train_data %>% select(where(is.numeric))
summary(num_col)
# Display the correlation matrix as a heatmap
cor_matrix <- cor(num_col, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color")

#Task 2. Create a validation dataset
# setting seed to generate a reproducible random sampling
set.seed(100)

# dividing the dataset into an 80:20 ratio
spl <- createDataPartition(train_data$Admitted_Flag, p = 0.8, list = FALSE)

# selecting part of dataset which belongs to the 80%
train <- train_data[spl, ]

# selecting part of dataset which belongs to the 20%
test <- train_data[-spl, ]

# Store X and Y for later use.
x <- train %>% select(-Admitted_Flag)
y <- train$Admitted_Flag
str(x)
str(y)

# overview of training data
skimmed <- skim(train)
skimmed


#B. Preprocess the training data
# Count missing values in the training set
sum(is.na(train))

# Remove columns with excessive number of NA values
thresh <- 0.5    # Define threshold for NA values

#Identify columns in training set with more than 50% NA
colnames(train[which(colMeans(is.na(train)) > thresh)])

# drop columns with more than 50% NA
train <- train[, which(colMeans(!is.na(train)) > thresh)]

# data imputation for missing values
# Set any null values in the 'Sex' column of train to 0
train$Sex[is.na(train$Sex)] <- 0

# Set any null values in the 'Provider_Patient_Distance_Miles' column of train to the mean of the column
train$Provider_Patient_Distance_Miles[is.na(train$Provider_Patient_Distance_Miles)] <- mean(train$Provider_Patient_Distance_Miles, na.rm = TRUE)
test$Provider_Patient_Distance_Miles[is.na(test$Provider_Patient_Distance_Miles)] <- mean(test$Provider_Patient_Distance_Miles, na.rm = TRUE)

# Set any null values in the 'IMD_Decile_From_LSOA' column of train to 5
train$IMD_Decile_From_LSOA[is.na(train$IMD_Decile_From_LSOA)] <- 5
test$IMD_Decile_From_LSOA[is.na(test$IMD_Decile_From_LSOA)] <- 5

# # Set any null values of 'Length_Of_Stays_Days' to 0
# train$Length_Of_Stay_Days[is.na(train$Length_Of_Stay_Days)] <- 0
# test$Length_Of_Stay_Days[is.na(test$Length_Of_Stay_Days)] <- 0

# Replace 'NA' in "EA_HRG" with the value "Nothing"
train$AE_HRG[is.na(train$AE_HRG)] <- "Nothing"
test$AE_HRG[is.na(test$AE_HRG)] <- "Nothing"

# create One-Hot Encoding(dummy variables) and ordinal encoding for categorical variables
age_band_levels <- c("1-17", "18-24", "25-44", "45-64", "65-84", "85")
train$Age_Band <- as.integer(factor(train$Age_Band, levels = age_band_levels, ordered = TRUE))
test$Age_Band <- as.integer(factor(test$Age_Band, levels = age_band_levels, ordered = TRUE))

train <- dummy_cols(train, select_columns = "AE_HRG", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
test <- dummy_cols(test, select_columns = "AE_HRG", remove_first_dummy = TRUE, remove_selected_columns = TRUE)



# Normalize data formats

# Load required library

# Convert 'AE_Arrive_Date' to datetime format
train$AE_Arrive_Date <- as.POSIXct(train$AE_Arrive_Date)
test$AE_Arrive_Date <- as.POSIXct(test$AE_Arrive_Date)

# Extract date components
train <- train %>%
    mutate(Arrival_Year = year(AE_Arrive_Date),
           Arrival_Month = month(AE_Arrive_Date),
           Arrival_Day = day(AE_Arrive_Date),
           Arrival_DayOfWeek = wday(AE_Arrive_Date, label = FALSE) - 1)  # Monday=0, Sunday=6

test <- test %>%
   mutate(Arrival_Year = year(AE_Arrive_Date),
          Arrival_Month = month(AE_Arrive_Date),
          Arrival_Day = day(AE_Arrive_Date),
          Arrival_DayOfWeek = wday(AE_Arrive_Date, label = FALSE) - 1)  # Monday=0, Sunday=6

# Drop the original 'AE_Arrive_Date' field
train <- train %>% select(-AE_Arrive_Date)
test <- test %>% select(-AE_Arrive_Date)


#C. Reanalyse the new training set
# Summary Statistics
str(train)

# Missing values analysis
# Check missing values after data imputation
sum(is.na(train))
colSums(is.na(train))

# Visualize importance of variables using featurePlot()

featurePlot(x = train[, -which(names(train) %in% "Admitted_Flag")],
            y = train$target,
            plot = "density",
            scales = list(x = list(relation = "free"),
                          y = list(relation = "free")))

# Feature selection using recursive feature elimination(rfe)

# Check for NA values in the predictors
sum(is.na(train[, -which(names(train) == "Admitted_Flag")]))

# Say goodbye to 10285 rows with NA values. rip.
train <- train[complete.cases(train[, -which(names(train) == "Admitted_Flag")]), ]


ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
result <- rfe(train[, -which(names(train) == "Admitted_Flag")], train$Admitted_Flag, sizes=c(1:ncol(train)-1), rfeControl=ctrl)
print(result)


#E. Evaluate ML algorithms
#1. Build models
#2. Select best model
#3. Compare accuracy using visualizations
#4. Summarise best model
#5. rank features by importance


#F. Make predictions using test data and the most accurate model.


#G. Create ensemble predictions for improved accuracy (Stretch goal)
# - Bagging
# - Boosting
# - Stacking
