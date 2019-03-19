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

temp = cbind(Tin=temp[,1], HTC=temp[,3]/(temp[,1]-temp[,2]), 
             HTCd=temp[,3]/(temp[,1]-temp[,2])*diff(temp[,1]),
             Srd = temp[,4])
temp = ts(temp[-1,]) #Remove the first line and stop it form becoming matrix

train <- subset(temp, end=length(y)- 72)
test <- subset(temp, start=length(y) - 71)
length(train); length(test)
head(train); head(test[,-1])

#SIMPLE LINEAR REGRESSION PREDICTION
slin_pred <- lm(Tin ~ HTC, data=train)
summary(slin_pred)
slinpred <- predict(slin_pred, newdata=data.frame(test[,-1]))
class(slinpred)
length(slinpred)
autoplot(ts(cbind(slinpred,test[,1])))
a = max(abs(slinpred - test[,1]))

#MULTIPLE LINEAR REGRESSION PREDICTION
lmfit <- lm(Tin ~ HTC + HTCd + Srd, train) 
tslmpr <- predict(lmfit, newdata = data.frame(test[,-1]))
autoplot(ts(cbind(tslmpr,test[,1])))
summary(lmfit)
b <- max(abs(tslmpr - test[,1]))

#SUPPORT VECTOR MACHINE REGRESSION (SVR)
library(e1071)
svm_fit <- svm(Tin ~ ., train, type='eps-regression')
svm_pr <- predict(svm_fit,newdata = data.frame(test[,-1]))
autoplot(ts(cbind(svm_pr, test[,1])))
c = max(abs(svm_pr - test[,1]))


#DECISION TREE REGRESSION
library(rpart)
dtfit <- rpart(Tin ~ ., data=train, control=rpart.control(minsplit = 1))
dtpr <- predict(dtfit, newdata = data.frame(test[,-1]))
autoplot(ts(cbind(dtpr, test[,1])))
summary(dtfit)
d = max(abs(dtpr - test[,1]))

#RANDOM FOREST PREDICTION
library(randomForest)
rffit <- randomForest(Tin ~ ., data=train, ntree=1500)
rfpr <- predict(rffit, newdata = data.frame(test[,-1]))
autoplot(ts(cbind(rfpr, test[,1])))
e = max(abs(rfpr - test[,1]))

#POLYNOMIAL REGRESSION PREDICTION
head(temp[,1:2])

dataset = cbind(Tin=temp[,1], Tout=temp[,2], CoT= temp[,2]^0.333,  RoT=temp[,2]^0.5, ToS=temp[,2]^2, ToC=temp[,2]^3, ToQ=temp[,2]^4, To5=temp[,2]^5)

train <- subset(dataset, end=length(y)- 72)
test <- subset(dataset, start=length(y) - 71)

poly_fit <- tslm(Tin ~ ToC + ToQ, train) 
polyfc <- forecast(poly_fit, newdata = data.frame(test[,-1]))
autoplot(polyfc) + autolayer(test[,1], series='Actual')
autoplot(polyfc) + autolayer(test[,1])
f = max(abs(polyfc$mean - test[,1]))
summary(poly_fit)

Method <- c('Simple Linear','Multiple Linear','SVM','Decision Tree','Random Forest','Polynomial')
Result <- c(a,b,c,d,e,f)
df_result <- data.frame(Method, Result)
df_result[order(df_result[,'Result']),]