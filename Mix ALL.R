temp <- read.csv('temp_data.csv')
temp$Srd <- ifelse(temp$Srd>10,temp$Srd,0)
head(temp)

###Testing the methods
library(fpp2)
library(astsa)
temp <-ts(temp[,-1], frequency = 24)
plot(temp)
y = temp[,1]

length(y)

train <- subset(temp, end=length(y)- 72)
test <- subset(temp, start=length(y) - 71)
length(train); length(test)
h = length(test)

#ARIMA using auto.arima - This the best model so far
arimafit <- auto.arima(train[,1], xreg=train[,-1])
arimafc <- forecast(arimafit, xreg=test[,-1])
autoplot(arimafc) + autolayer(test[,1], series='Actual')
mean(abs(arimafc$mean - test[,1]))
summary(arimafit)

autoplot(arimafc) + autolayer(test[,1], series='Actual')
checkresiduals(arimafit)

BoxCox.lambda(train[,1])
arima_fit <- auto.arima(train[,1], lambda = 1.999924)
arima_fc <- forecast(arima_fit, h=24*3)
checkresiduals(arima_fit)
autoplot(arimafc) + autolayer(test[,1], series='Actual')
max(abs(arima_fc$mean - test[,1]))
summary(arima_fit)

#Mean
meanfit <- meanf(train, h=h)
autoplot(meanfit)
#Naive and Snaive
naivefit <- naive(train, h = h)
autoplot(naivefit) + autolayer(test, series='Actual')
snaivefit <- snaive(train, h=h)
autoplot(snaivefit) + autolayer(test, series='Actual')#Snaive fit gives similar pattern
length(snaivefit)
#Simple Exponential Smoothing
sesfit <- ses(train, h = h)
length(sesfit) #only 10
autoplot(sesfit) + autolayer(test, series='Actual')
#Holt method both Linear and damped
holtfit <- holt(train, h = h) 
autoplot(holtfit) + autolayer(test, series='Actual')
holtdfit <- holt(train, h=h, damped = T)
autoplot(holtdfit) + autolayer(test, series='Actual')
#Holt Winds method, additive and multiplicative
hwfit <- hw(train, h = h)
autoplot(hwfit)#small looks additive - correct. confirmed.
hwfit <- hw(train, h=h, seasonal = 'additive')
autoplot(hwfit) + autolayer(test, series='Actual')
hwmfit <- hw(train, h=h, seasonal = 'multiplicative')
autoplot(hwmfit) + autolayer(test, series='Actual')
#Error, Trend and Seasonality
etsfit <- ets(train)
etsfc <- forecast(etsfit, h = h)
autoplot(etsfc) + autolayer(test, series='Actual')
#ARIMA using auto.arima
arimafit <- auto.arima(train)
arimafc <- forecast(arimafit, h=h)
autoplot(arimafc) + autolayer(test, series='Actual')
#tslm
tslmfit <- tslm(train ~ fourier(train, K=11))
tslmfc <- forecast(tslmfit, newdata = data.frame(fourier(train, K=11),h=h))
autoplot(tslmfc) + autolayer(test, series='Actual')
#tbats
tbatsfit <- tbats(train)
tbatsfc <- forecast(tbatsfit, h=h)
autoplot(tbatsfc) + autolayer(test, series='Actual')

a = accuracy(meanfit , test)['Test set','RMSE']
b = accuracy(naivefit , test)['Test set','RMSE']
c = accuracy(snaivefit , test)['Test set','RMSE']
d = accuracy(sesfit , test)['Test set','RMSE']
e = accuracy(holtfit , test)['Test set','RMSE']
f = accuracy(holtdfit , test)['Test set','RMSE']
g = accuracy(hwfit , test)['Test set','RMSE']
h = accuracy(hwmfit , test)['Test set','RMSE']
i = accuracy(etsfc , test)['Test set','RMSE']
j = accuracy(arimafc , test[,1])['Test set','RMSE']
k = accuracy(tslmfc , test[,1])['Test set','RMSE']
l = accuracy(tbatsfc , test)['Test set','RMSE']

Method <- c('meanf','naive','snaive','ses','holt','holtd','hw','hwm','ets','arima','tslm','tbats')
Result <- c(a,b,c,d,e,f,g,h,i,j,k,l)
#result <- c(meanf=a,naive=b,snaive=c,ses=d,holt=e,holtd=f,hw=g,ets=h,arima=i,tslm=j,tbats=k)
#matrix(result[order(result)])
df_result <- data.frame(Method, Result)
df_result[order(df_result[,'Result']),]

#############ARIMA
target <- temp[,2]
target <- ts(target, frequency = 6)
plot(target)
x = subset(target, end=length(target)-72) 
y = subset(target, start=length(target)-71) 


#Forecast the data 3 years into the future
fit <- sarima(x, 1,0,0,0,1,0,24, )
fc <- sarima.for(x, n.ahead=72, 1,0,0,0,1,1,24)
lines(y)
mean(abs(fc$pred[1:24] - y[1:24]))
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


#Find out how dynamic thermal simulations work