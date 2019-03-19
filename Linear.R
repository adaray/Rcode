#Importing Dataset
library(astsa)
library(fpp2)

dataset <- read.csv('temp_data.csv')
dataset$Srd <- ifelse(dataset$Srd>10,dataset$Srd,0)
dataset <- dataset[,2:5]


train <- dataset[1:504,]12
test <- dataset[505:720,]

fit <- lm(Tin ~ Tout+GasUsed+Srd, train)

#Splitting data into Training and Testing Set
library(caTools)
set.seed(123)
split = sample.split(dataset$Tin, SplitRatio = 0.8)
training_set = subset(dataset, split==T)
test_set = subset(dataset, split==F)

#Fitting Multiple Linear Regression to Training Data
regressor = lm(Tin ~ ., train)
summary(regressor)

y_pred = predict(fit, newdata=test)

max(abs(y_pred - test[,1]))

plot(y_pred, test[,1])

plot.ts(cbind(y_pred, test[,1]))
max(abs(y_pred - test[,1]))

regressor = lm(Profit ~ R.D.Spend+Administration+Marketing.Spend + State, 
               dataset)
summary(regressor)

regressor = lm(Profit ~ R.D.Spend+Administration+Marketing.Spend, 
               dataset)
summary(regressor)

regressor = lm(Profit ~ R.D.Spend+Marketing.Spend, 
               dataset)
summary(regressor)
regressor = lm(Profit ~ R.D.Spend, 
               dataset)
summary(regressor)


