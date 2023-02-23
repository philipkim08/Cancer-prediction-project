library(ggplot2)
library(tidyverse)

rm(list=ls()) #removes all previously stored variables
library(Hmisc)

data <- read.csv("~/Desktop/R datasets/breast-cancer.csv")
data$id <- NULL

# only include complete cases
data <- data[complete.cases(data),]

# create factors for malignant and benign cases
data$diagnosis <- factor(ifelse(data$diagnosis == "B", "benign", "malignant"))

# create test and training sets
trainingset <- data[1:341, 2:31]
testset <- data[342:569, 2:31]

# create test and training set outcomes

trainingsetoutcome <- data[1:341, 1]
testsetoutcome <- data[342:569, 1]

# apply KNN algorithm to the training and test outcomes
library(class)
predictions <- knn(train = trainingset, cl = trainingsetoutcome,
                   k = 18, test = testset)

# create a table with the predictions and the test set's outcomes
t = table(predictions, testsetoutcome)

# coerce the table into a data frame so that we can plot it using ggplot
t <- as.data.frame(t)

# using a double bar graph to display the predictions vs true outcomes from the test set
ggplot(t, aes(x = predictions, y = Freq, fill = testsetoutcome)) + 
  geom_bar(stat = 'identity') +
  labs(title = 'Predictions vs True Outcomes', y = 'Frequency', x = 'Predictions') +
  scale_fill_discrete(name = "State of Cancer", labels = c('Bengin', 'Malignant'))

