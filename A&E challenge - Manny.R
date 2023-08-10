#Code to explore A&E data and predict whether patients have been admitted.

# Install relevant packages
#install.packages('caret','tidyverse','caTools')
#install.packages("ggcorrplot")

# Load required packages
library(tidyverse)
library(caret)
library(caTools)
library(dplyr)


#A. Import all relevant files & read into dataframes
#download.file(url = "",destfile = "*.csv")
file_path <- "bip-ae-technical-challenge\\training_set.csv"
train_data <- read.csv(file_path)
test_path <- "bip-ae-technical-challenge\\test_set.csv"
test_data <- read.csv(test_path)


#Task 1. Explore the datasets
# Checking dimensions
dim(train_data)
dim(test_data)

# Summary statistics
str(train_data)
summary(train_data)
sample(train_data)

# Visualize variable distributions
# Histogram for numeric variables
num_col = train_data %>% select(where(is.numeric))
str(num_col)

for (i in 1:ncol(num_col)) {
  hist(num_col[[i]],
       col = "blue", ylim = c(0, 80000))
}

# Bar plots for categorical variables
var_col = train_data %>% select(!where(is.numeric))
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
# Display the correlation matrix as a heat map
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


#Task 3. Preprocess the training data
# Count missing values in the training set
sum(is.na(train))

# Remove columns with excessive number of NA values
thresh <- 0.5    # Define threshold for NA values

#Identify columns with more than 50% NA
colnames(train[which(colMeans(is.na(train)) > thresh)])

# Data imputation for missing values
# Set any null values in the 'Sex' column of train to 0
train$Sex[is.na(train$Sex)] <- 0

# Set any null values in the 'Provider_Patient_Distance_Miles' 
# column of train to the mean of the column
train$Provider_Patient_Distance_Miles[is.na(train$Provider_Patient_Distance_Miles)] <- mean(train$Provider_Patient_Distance_Miles, na.rm = TRUE)
test$Provider_Patient_Distance_Miles[is.na(test$Provider_Patient_Distance_Miles)] <- mean(test$Provider_Patient_Distance_Miles, na.rm = TRUE)

# Set any null values in the 'IMD_Decile_From_LSOA' column to 5
train$IMD_Decile_From_LSOA[is.na(train$IMD_Decile_From_LSOA)] <- 5
test$IMD_Decile_From_LSOA[is.na(test$IMD_Decile_From_LSOA)] <- 5

# # Set any null values of 'ICD10_Chapter_Code' to 'OTHER''
train$ICD10_Chapter_Code[is.na(train$ICD10_Chapter_Code)] <- "OTHER"
test$ICD10_Chapter_Code[is.na(test$ICD10_Chapter_Code)] <- "OTHER"

# Replace 'NA' in "EA_HRG" with the value "Nothing"
train$AE_HRG[is.na(train$AE_HRG)] <- "Nothing"
test$AE_HRG[is.na(test$AE_HRG)] <- "Nothing"

# # Set any null values of 'Treatment_Function_Code' to 'OTHER''
train$Treatment_Function_Code[is.na(train$Treatment_Function_Code)] <- "OTHER"
test$Treatment_Function_Code[is.na(test$Treatment_Function_Code)] <- "OTHER"

# Set any null values in the 'IMD_Decile_From_LSOA' column to 5
train$IMD_Decile_From_LSOA[is.na(train$IMD_Decile_From_LSOA)] <- 5
test$IMD_Decile_From_LSOA[is.na(test$IMD_Decile_From_LSOA)] <- 5

# Set any null values of 'Length_Of_Stay_Days' to a random integer between 1-45
train$Length_Of_Stay_Days[is.na(train$Length_Of_Stay_Days)] <- sample(1:45, sum(is.na(train$Length_Of_Stay_Days)), replace=TRUE)
test$Length_Of_Stay_Days[is.na(test$Length_Of_Stay_Days)] <- sample(1:45, sum(is.na(test$Length_Of_Stay_Days)), replace=TRUE)


# create One-Hot Encoding(dummy variables) and 
# ordinal encoding for categorical variables

# Normalize age & datetime fields

#Task 4. Re-analyse the new training set
# Summary Statistics
str(train)

# Missing values analysis
# Check missing values after data imputation
sum(is.na(train))
colSums(is.na(train))

# Estimate variable importance

# Feature selection
# Check for NA values in the predictors
sum(is.na(train[, -which(names(train) == "Admitted_Flag")]))

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
