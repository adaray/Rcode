#Importing Dataset
dataset <- read.csv('data.csv')

#Dealing with Missing Data
dataset$Age <- ifelse(is.na(dataset$Age), mean(dataset$Age, na.rm=T), dataset$Age)
dataset$Salary <- ifelse(is.na(dataset$Salary), mean(dataset$Salary, na.rm=T), dataset$Salary)

#Encoding Categorical Data
dataset$Country <- factor(dataset$Country, 
                          levels = c('France','Spain','Germany'),
                          labels = c(1,2,3))

dataset$Purchased <- factor(dataset$Purchased, 
                          levels = c('No','Yes'),
                          labels = c(0,1))

#Splitting data into Training and Testing Set
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set = subset(dataset, split==T)
test_set = subset(dataset, split==F)

#Feature Scaling
training_set[,2:3] = scale(training_set[,2:3])
test_set[,2:3] = scale(test_set[,2:3])