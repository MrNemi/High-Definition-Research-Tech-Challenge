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

# Add clean training set here

set.seed(64)
rows <- sample(nrow(training_set))
shuffled_training <- training_set[rows, ]

split <- round(nrow(shuffled_training)* 0.8)
train1 <- training_set[1:split, ]
test1 <- test_set[(split + 1):nrow(test_set), ]



# Training model: Logistic regression 

model <- lm(Admitted_Flag ~., train1)
p <- predict(model, final_clean)
error <- p - final_clean[['Admitted_Flag']]
sqrt(mean(error ^ 2))

# SCORE = 0.4262178

model3 <- glm(Admitted_Flag ~., data=training_set)
summary(model3)

# Fisher’s Scoring Algorithm needed one iterations to perform the fit.

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

