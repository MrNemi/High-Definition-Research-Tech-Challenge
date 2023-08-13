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

library(tidyverse)
colSums(is.na(training_set))
colSums(is.na(test_set))

# Visualise which data is missing
install.packages('Amelia')
library(Amelia)

missmap(training_set, y.labels = NULL, y.at = NULL, col = c("orange", "lightblue"), 
        main = "Missing Values vs Observed")

# Add Admitted_Flag column to test set

test_set$Admitted_Flag <- NA
view(test_set)

# One hot encoding using dummyVars in caret package

install.packages("fastDummies")
library(fastDummies)

library(caret)
age_band_levels <- c("1-17", "18-24", "25-44", "45-64", "65-84", "85")
training_set$Age_Band <- as.integer(factor(training_set$Age_Band, levels = age_band_levels, ordered = TRUE))
test_set$Age_Band <- as.integer(factor(test_set$Age_Band, levels = age_band_levels, ordered = TRUE))

trainN <- dummy_cols(training_set, select_columns = "AE_HRG", remove_first_dummy = TRUE, remove_selected_columns = TRUE)
testN <- dummy_cols(test_set, select_columns = "AE_HRG", remove_first_dummy = TRUE, remove_selected_columns = TRUE)

view(trainN)
view(testN)

# Convert to datetime format

install.packages("lubridate")
library(lubridate)

trainN$AE_Arrive_Date <- as.POSIXct(trainN$AE_Arrive_Date)
testN$AE_Arrive_Date <- as.POSIXct(testN$AE_Arrive_Date)

# Extract date components
trainQ <- trainN %>%
  mutate(Arrival_Year = year(AE_Arrive_Date),
         Arrival_Month = month(AE_Arrive_Date),
         Arrival_Day = day(AE_Arrive_Date),
         Arrival_DayOfWeek = wday(AE_Arrive_Date, label = FALSE) - 1)  # Monday=0, Sunday=6

testQ <- testN %>%
  mutate(Arrival_Year = year(AE_Arrive_Date),
         Arrival_Month = month(AE_Arrive_Date),
         Arrival_Day = day(AE_Arrive_Date),
         Arrival_DayOfWeek = wday(AE_Arrive_Date, label = FALSE) - 1)  # Monday=0, Sunday=6

# Drop the original 'AE_Arrive_Date' field
trainZ <- trainQ %>% select(-AE_Arrive_Date)
testZ <- testQ %>% select(-AE_Arrive_Date)

view(trainZ)
View(testZ)

colSums(is.na(trainZ))
colSums(is.na(testZ))

# add admitted_flag column to test set

testZ$Admitted_Flag <- NA
view(testZ)


# Replace Na's in test set with 0

testZ$Provider_Patient_Distance_Miles[is.na(testZ$Provider_Patient_Distance_Miles)] <- mean(testZ$Provider_Patient_Distance_Miles, na.rm = TRUE)
testZ$IMD_Decile_From_LSOA[is.na(testZ$IMD_Decile_From_LSOA)] <- 5
testZ$Length_Of_Stay_Days[is.na(testZ$Length_Of_Stay_Days)] <- 0
testZ$Admitted_Flag[is.na(testZ$Admitted_Flag)] <- 0
testZ$ICD10_Chapter_Code[is.na(testZ$ICD10_Chapter_Code)] <- 0
testZ$Sex[is.na(testZ$Sex)] <- 0
testZ$Age_Band[is.na(testZ$Age_Band)] <- 0
testZ$AE_Arrive_HourOfDay[is.na(testZ$AE_Arrive_HourOfDay)] <- 0
testZ$Treatment_Function_Code[is.na(testZ$Treatment_Function_Code)] <- 0
testZ$AE_HRG_Medium[is.na(testZ$AE_HRG_Medium)] <- 0
testZ$AE_HRG_Nothing[is.na(testZ$AE_HRG_Nothing)] <- 0
testZ$AE_HRG_Low[is.na(testZ$AE_HRG_Low)] <- 0


view(testZ)
colSums(is.na(testZ))

trainZ$Provider_Patient_Distance_Miles[is.na(trainZ$Provider_Patient_Distance_Miles)] <- mean(testZ$Provider_Patient_Distance_Miles, na.rm = TRUE)
trainZ$IMD_Decile_From_LSOA[is.na(trainZ$IMD_Decile_From_LSOA)] <- 5
trainZ$Length_Of_Stay_Days[is.na(trainZ$Length_Of_Stay_Days)] <- 0
trainZ$ICD10_Chapter_Code[is.na(trainZ$ICD10_Chapter_Code)] <- 0
trainZ$Sex[is.na(trainZ$Sex)] <- 0
trainZ$Age_Band[is.na(trainZ$Age_Band)] <- 0
trainZ$AE_Arrive_HourOfDay[is.na(trainZ$AE_Arrive_HourOfDay)] <- 0
trainZ$Treatment_Function_Code[is.na(trainZ$Treatment_Function_Code)] <- 0
trainZ$AE_HRG_Medium[is.na(trainZ$AE_HRG_Medium)] <- 0
trainZ$AE_HRG_Nothing[is.na(trainZ$AE_HRG_Nothing)] <- 0
trainZ$AE_HRG_Low[is.na(trainZ$AE_HRG_Low)] <- 0


view(trainZ)
colSums(is.na(trainZ))


# Use predictive power score to form a heat map

install.packages('ppsr')
library(ppsr)

ppsr::visualize_pps(df = training_set, y = 'Admitted_Flag') +
  ggplot2::theme_classic() +
  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white")) +
  ggplot2::theme(title = ggplot2::element_text(size = 10)) +
  ggplot2::labs(title = 'Variables Predictive Power Score', 
                x = 'Predictive Power Score',
                y = 'Variables')

# Remove columns that do not possess predictive power.

clean_training_set <- training_set %>% select(Admitted_Flag,AE_Num_Investigations,
                                              AE_Time_Mins, AE_Arrival_Mode,
                                              AE_HRG)

# Forming a correlation matrix


ppsr::visualize_correlations(df = clean_training_set) +
  ggplot2::theme_classic() +
  ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white")) +
  ggplot2::theme(title = ggplot2::element_text(size = 10)) +
  ggplot2::labs(title = 'Variables Correlation', 
                x = 'Correlation Between Variables',
                y = 'Variables')

# using correlation matrix see Arrival mode to have low correlation so also remove

final_clean <- clean_training_set %>% select(Admitted_Flag,AE_Num_Investigations,
                                             AE_Time_Mins)
colSums(is.na(final_clean))
view(final_clean)

## Data Visualization in R
#see the columns of the dataset and display some portion of the data 
glimpse(training_set)
glimpse(test_set)


# seperate data into numeric to form graphs



# Length of stay plotted via ggplot2

dfLength <- select(training_set, AE_Arrive_Date, Length_Of_Stay_Days)
view(dfLength)

dfLength1 <- na.omit(dfLength)
view(dfLength1)


ggplot(dfLength1, aes(x=AE_Arrive_Date, y= Length_Of_Stay_Days)) +
  geom_bar(stat ='identity') +
  geom_line(colour = 'lightblue') + 
  ggtitle('How long each patient stayed in A&E per year')

#  Use Age range to show which age band has highest A&E admissions
# make sure age ranges are collated



# which time has the most A&E admissions

dfTime <- select(training_set, AE_Time_Mins, AE_Arrive_Date) 

# change time to hours
# also for dates

ggplot(dfTime, aes(x=AE_Arrive_Date, y= AE_Time_Mins)) +
  geom_bar(stat ='identity') +
  geom_line(colour = 'lightgreen') + 
  ggtitle('Which year had the longest time a patient would be in A&E')

## Forming ML Model
# I saw where they combined both data, train & test for easier manipulation?
# Then split into test and train after data cleaning

# Forming Validation Data set for later use

set.seed(64)
rows <- sample(nrow(final_clean))
shuffled_training <- final_clean[rows, ]

split <- round(nrow(shuffled_training)* 0.8)
trainR <- final_clean[1:split, ]

view(trainR)


# Training model: Logistic regression 

model <- lm(Admitted_Flag ~ ., final_clean[1:6],)
p <- predict(model, testZ)
error <- p - final_clean[['Admitted_Flag']]
sqrt(mean(error ^ 2))

model2 <- glm(Admitted_Flag ~., data=final_clean)
summary(model)

view(final_clean)

# SCORE = 0.7005988

model2 <- glm(Admitted_Flag ~., data=final_clean)
summary(model2)

# Fisher’s Scoring Algorithm needed two iterations to perform the fit.

# After making the model, can see how accurate it is

install.packages('ROCR')
library(ROCR)
predict <- predict(model, newdata=subset(final_clean,select=c(2,3)), type="response")
pr <- prediction(p, final_clean$Admitted_Flag)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# 0.8287476 <- ROC traces the percentage of true positives accurately predicted by a given logit model, a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5

# Class Predictions
pred_class <- predict(model,
                      new_data = final_clean,
                      type = "class")

# Class Probabilities - ADD CLEAN TEST
pred_proba <- predict(model,
                      new_data = test,
                      type = "prob")


results <- final_clean %>%
  select(Admitted_Flag) %>%
  bind_cols(pred_class, pred_proba)

accuracy(results, truth = y, estimate = Admitted_Flag)

# The Akaike Information Criterion (AIC)



# Hosmer-Lemeshow Goodness of Fit



# Deviance

# Add CLEAN TEST

library(caret)
confusionMatrix(final_clean$Admitted_Flag, reference = test_set, threshold = optCutOff)

# Finally use cross validation


# Training model using Random Forest
install.packages('randomForest')
library(randomForest)

# Create Random Forest Model
model4 <- randomForest(Admitted_Flag~.,data= final_clean)
importance(model4)

pred_test <- predict(model4, newdata = final_clean, type= "class")

# make a confusion matrix 

# Results from important() -> IncNodePurity
# AE_Num_Investigations      5180.312
# AE_Time_Mins               4176.912
# meaning a higher score means that the specific feature will have a larger effect on the model that is being used to predict a certain variable

