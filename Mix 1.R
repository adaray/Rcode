temp <- read.csv('temp_data.csv')
temp$Srd <- ifelse(temp$Srd>10,temp$Srd,0)
head(temp)
library(fpp2)
library(astsa)
temp <- ts(temp[,c(2:5)], frequency = 24)

plot(temp)
y = temp[,1]
plot(y)
length(y)

train <- subset(temp, end=length(y)- 72)
test <- subset(temp, start=length(y) - 71)
length(train); length(test)
head(train); head(test[,-1])
class(train)

#SIMPLE LINEAR REGRESSION FORECAST
simple_linear <- tslm(Tin ~ Tout, data=train)
summary(simple_linear)
slinfc <- forecast(simple_linear, newdata=data.frame(test[,-1]))
class(slinfc)
length(slinfc) #This is a forecast, thus it has 11 elements. Use mean value
autoplot(slinfc) + autolayer(test[,1])
mean(abs(slinfc$mean - test[,1]))

#SIMPLE LINEAR REGRESSION PREDICTION
slin_pred <- lm(Tin ~ Tout, data=train)
summary(slin_pred)
slinpred <- predict(slin_pred, newdata=data.frame(test[,-1]))
class(slinpred)
length(slinpred)
autoplot(ts(cbind(slinpred,test[,1])))
mean(abs(slinpred - test[,1]))

#MULTIPLE LINEAR REGRESSION FORECAST
tslm_fit <- tslm(Tin ~ Tout + GasUsed + Srd, train) 
tslmfc <- forecast(tslm_fit, newdata = data.frame(test[,-1]))
autoplot(tslmfc) + autolayer(test[,1], series='Actual')
summary(tslm_fit)
mean(abs(tslmfc$mean - test[,1]))

#MULTIPLE LINEAR REGRESSION PREDICTION
lmfit <- lm(Tin ~ Tout + GasUsed + Srd, train) 
tslmpr <- predict(lmfit, newdata = data.frame(test[,-1]))
autoplot(ts(cbind(tslmpr,test[,1])))
summary(lmfit)
mean(abs(tslmpr - test[,1]))

#SUPPORT VECTOR MACHINE REGRESSION (SVR)
library(e1071)
svm_fit <- svm(Tin ~ ., train, type='eps-regression')
svm_pr <- predict(svm_fit,newdata = data.frame(test[,-1]))
autoplot(ts(cbind(svm_pr, test[,1])))
mean(abs(svm_pr - test[,1]))


#DECISION TREE REGRESSION
library(rpart)
dtfit <- rpart(Tin ~ ., data=train, control=rpart.control(minsplit = 1))
dtpr <- predict(dtfit, newdata = data.frame(test[,-1]))
autoplot(ts(cbind(dtpr, test[,1])))
summary(dtfit)
mean(abs(dtpr - test[,1]))

#RANDOM FOREST PREDICTION
library(randomForest)
rffit <- randomForest(Tin ~ ., data=train, ntree=100)
rfpr <- predict(rffit, newdata = data.frame(test[,-1]))
autoplot(ts(cbind(rfpr, test[,1])))
mean(abs(rfpr - test[,1]))

#POLYNOMIAL REGRESSION PREDICTION
head(temp[,1:2])

dataset = cbind(Tin=temp[,1], Tout=temp[,2], CoT= temp[,2]^0.333,  RoT=temp[,2]^0.5, ToS=temp[,2]^2, ToC=temp[,2]^3, ToQ=temp[,2]^4, To5=temp[,2]^5)

train <- subset(dataset, end=length(y)- 72)
test <- subset(dataset, start=length(y) - 71)

poly_fit <- tslm(Tin ~ ToC + ToQ, train) 
polyfc <- forecast(poly_fit, newdata = data.frame(test[,-1]))
autoplot(polyfc) + autolayer(test[,1], series='Actual')
autoplot(polyfc) + autolayer(test[,1])
mean(abs(polyfc$mean - test[,1]))
summary(poly_fit)

Method <- c('Simple Linear','Multiple Linear','SVM','Decision Tree','Random Forest','Polynomial')
Result <- c(a,b,c,d,e,f)
df_result <- data.frame(Method, Result)
df_result[order(df_result[,'Result']),]



#Modifying the data and adding extra temp diff column

df <- read.csv('temp_data.csv')
df$Srd <- ifelse(df$Srd>10,df$Srd,0)
df <- cbind(df[-1,-1], Tind = df$Tin[-720] - df$Tin[-1])
df <- ts(df, frequency = 1)
y = df[,1]

train <- subset(df, end=length(y)-72)
test<- subset(df, start=length(y) - 71)

###########ARX Model
tslm_fit <- tslm(Tin ~ Tout + GasUsed + Srd + Tind, train)
tslm_fc <- forecast(tslm_fit, newdata = data.frame(test[,-1]))
summary(tslm_fit)
autoplot(tslm_fc) + autolayer(test[,1], series='Actual')
mean(abs(tslm_fc$mean - test[,1]))
#Remove Srd because it is no significant
tslm_fit <- tslm(Tin ~ Tout + GasUsed + Tind, train)
tslm_fc <- forecast(tslm_fit, newdata = data.frame(test[,-1]))
summary(tslm_fit)
autoplot(tslm_fc) + autolayer(test[,1], series='Actual')
mean(abs(tslm_fc$mean - test[,1]))

####Random Forest model
library(randomForest)
n=c(10,50,100,150,250,500,1000,1500)

ranf_fit <- randomForest(Tin ~ Tout + GasUsed + Srd + Tind, train, ntree=50)
ranf_fc <- predict(ranf_fit, newdata = data.frame(test[,-1]))
mean(abs(ranf_fc - test[,1]))
autoplot(cbind(ranf_fc, test[,1]))
cbind(ranf_fc, test[,1])

#Using ARIMA on this modified dataset - frequency is 24
arimafit <- auto.arima(train[,1], xreg=train[,-1])
arimafc <- forecast(arimafit, xreg=test[,-1])
autoplot(arimafc) + autolayer(test[,1], series='Actual')
mean(abs(arimafc$mean - test[,1]))
summary(arimafit)

#Using ARIMA on this modified dataset - frequency is 1
arimafit <- auto.arima(train[,1], xreg=train[,-1])
arimafc <- forecast(arimafit, xreg=test[,-1])
autoplot(arimafc) + autolayer(test[,1], series='Actual')
mean(abs(arimafc$mean - test[,1]))
summary(arimafit)
cbind(arimafc$mean, test[,1])


#############SARIMA FORECASTING
target <- temp[,2]
target <- ts(target, frequency = 168)
plot(target)
x = subset(target, end=length(target)-72) 
y = subset(target, start=length(target)-71) 


#Forecast the data 3 years into the future
fit <- sarima(x, ,1,0,0,1,0,24)
fc <- sarima.for(x, n.ahead=72, 4,1,4,0,1,0,168)
plot(fc)
lines(y)
fit$ttable

plot(x)
acf2(x)
diff_x <- diff(x)
plot(diff_x)
Box.test(diff_x)

diffl_x <- diff(diff_x, 24)
Box.test(diffl_x)
plot(diffl_x)
acf2(diffl_x)

auto.arima(x)
