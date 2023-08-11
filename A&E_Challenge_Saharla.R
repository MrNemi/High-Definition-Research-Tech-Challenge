# Installing packages & opening up their libraries
install.packages('dplyr')
install.packages("tidyverse")
install.packages('caret')

library(dplyr)
library(tidyverse)
library(caret)

# setting working directory
setwd("~/bip-ae-technical-challenge")

# Importing data
library(readxl)
Data_Dictionary <- read_excel("Data_Dictionary.xlsx")
View(Data_Dictionary)

library(readr)
example_answer_sheet <- read_csv("example_answer_sheet.csv")
View(example_answer_sheet)

skinny_unanswered_set <- read_csv("skinny_unanswered_set.csv")
View(skinny_unanswered_set)

test_set <- read_csv('test_set.csv')
View(test_set)

training_set <- read_csv("training_set.csv")
View(training_set)

## Cleaning Data in R
# See which columns have missing values
colSums(is.na(training_set))
colSums(is.na(test_set))

# Visualise which data is missing
install.packages('Amelia')
library(Amelia)

missmap(training_set, y.labels = NULL, y.at = NULL, col = c("orange", "lightblue"), 
        main = "Missing Values vs Observed")


# Remove columns with missing values and ID columns? Don't possess predictive power.
# Use predictive power score to form a heat map

library(tidyverse)

#see the columns of the dataset and display some portion of the data 
glimpse(training_set)
glimpse(test_set)




## Data Visualization in R
# seperate data into numeric to form graphs

# Length of stay plotted via ggplot2


#  Use Age range to show which age band has highest A&E admissions



# which time has the most A&E admissions


## Forming ML Model
# I saw where they combined both data, train & test for easier manipulation?
# Then split into test and train after data cleaning

# Add clean training set here

set.seed(64)
rows <- sample(nrow(training_set))
shuffled_training <- training_set[rows, ]

split <- round(nrow(shuffled_training)* 0.8)
train1 <- training_set[1:split, ]
test1 <- test_set[(split + 1):nrow(test_set), ]



# Training model: Logistic regression or random forest?







# After making the model, can see how accurate it is

install.packages('ROCR')
library(ROCR)





# Tuning the model using cross validation




