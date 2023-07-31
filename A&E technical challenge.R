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
file_path <- "bip-ae-technical-challenge//training_set.csv"
train_data <- read.csv(file_path)
test_path <- "bip-ae-technical-challenge//test_set.csv"
test_data <- read.csv(test_path)


#Task 1. Explore the datasets
# Checking dimensions
dim(train_data)
dim(test_data)

# Summary statistics
str(train_data)
summary(train_data)

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
num_col = train_data %>% select(where(is.numeric))
summary(num_col)
# Display the correlation matrix as a heatmap




#Task 2. Create a validation dataset
# setting seed to generate a reproducible random sampling
set.seed(100)

# dividing the dataset into an 80:20 ratio
spl = createDataPartition(train_data$Admitted_Flag, p=0.8, list=FALSE)

# selecting part of dataset which belongs to the 80%
train = train_data[spl,]

# selecting part of dataset which belongs to the 20%
test = train_data[-spl,]

# Store X and Y for later use.
x = train %>% select(-Admitted_Flag)
y = train$Admitted_Flag
str(x)
str(y)

# overview of training data
skimmed <- skim(train)
skimmed


#B. Preprocess the training data
# Count missing values in the training set
sum(is.na(train))

# Remove columns with excessive number of NA values
thresh = 0.5    # Define threshold for NA values

#Identify columns in training set with more than 50% NA
colnames(train[which(colMeans(is.na(train)) > thresh)])

# drop columns with more than 50% NA
train <- train[, which(colMeans(!is.na(train)) > thresh)]

# data imputation for missing values


# create One-Hot Encoding(dummy variables)


# Normalize data formats



#C. Reanalyse the new training set
# Summary Statistics
#str(train)

# Missing values analysis
# Check missing values after data imputation
sum(is.na(train))
colSums(is.na(train))

# Visualize importance of variables using featurePlot()


# Feature selection using recursive feature elimination(rfe)


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
