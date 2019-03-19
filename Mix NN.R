library(fpp2)
plot(sunspotarea)
fit <- nnetar(sunspotarea, lambda = 0)
autoplot(forecast(fit, h=30))

temp <- read.csv('temp_data.csv')
train = subset(ts(temp[,2], frequency = 24), end=720-72)
test = subset(ts(temp[,2], frequency = 24), start=720-71)
length(train); length(test)

nn_fit <- nnetar(train)
nn_fc <- forecast(nn_fit, h=72)
autoplot(nn_fc) + autolayer(test, series='Actual')
max(abs(nn_fc$mean-test))
mean(abs(nn_fc$mean-test))



library(ffp2)
temp <- read.csv('temp_data.csv')
head(temp)
temp = ts(temp, frequency = 24)
temp = cbind(Tin=temp[-1,2], Tind=diff(temp[,2]),
             Tout = temp[-1,3], Gas = temp[-1,4],Srd = temp[-1,5])

train = subset(temp, end=720-72)
test = subset(temp, start=720-71)

#Multiple Regression
mult_fit <- tslm(Tin ~ Tind + Tout + Gas + Srd, train)
mult_fc <- forecast(mult_fit, newdata = data.frame(test[,-1]))
autoplot(mult_fc) + autolayer(test[,1], series='Actual')
mean(abs(mult_fc$mean - test[,1]))
summary(mult_fit)

#Multiple Regression - remove unnecessary variables
mult_fit <- tslm(Tin ~ Tind + Tout, train)
mult_fc <- forecast(mult_fit, newdata = data.frame(test[,-1]))
autoplot(mult_fc) + autolayer(test[,1], series='Actual')
mean(abs(mult_fc$mean - test[,1]))
summary(mult_fit)

#Random Forest
library(randomForest)
rf_fit <- randomForest(Tin ~ . , train, ntree=10)
rf_pr <- predict(rf_fit, newdata = data.frame(test[,-1]))
autoplot(cbind(rf_pr, test[,1]))
mean(abs(rf_pr - test[,1]))
