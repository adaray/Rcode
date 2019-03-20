#Import Libraries
library(ggplot2)
library(dplyr)
library(rpart)
library(e1071)
library(randomForest)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(neuralnet)
library(fpp2)
library(reshape2)

getwd()
setwd("C:/Users/Daray/Google Drive (a.ahmed.daray@gmail.com)/New Collection/cvaa12/CVP318/R code")

############################## TEMPERATURE DATA

temp <- read.csv('temp_data1.csv')
temp$Srd <- ifelse(temp$Srd>10,temp$Srd,0)
temp$DateTime = temp$Date.Time
#temp$DateTime <- as.POSIXct(temp$Date.Time, format="%m/%d/%Y %H:%M")
head(temp)

df <- data.frame(DateTime = as.POSIXct(temp$Date.Time, format="%m/%d/%Y %H:%M"), Tin=round(temp[,2],2))

for (i in 1:720){
  #Indoor Temperature now and the past three hours
  #df$Tin[i] <- round(temp$Tin[i],2)
  df$Tin_1[i+1] <- df$Tin[i]
  df$Tin_2[i+1] <- df$Tin_1[i]
  df$Tin_3[i+1] <- df$Tin_2[i]

  
  #Outdoor Temperature now and the past three hours
  df$Tout[i] <- round(temp$Tout[i],2)
  df$Tout_1[i+1] <- df$Tout[i]
  df$Tout_2[i+1] <- df$Tout_1[i]
  df$Tout_3[i+1] <- df$Tout_2[i]
  #GasUsed now and the past three hours
  df$Q[i] <- round(temp$GasUsed[i],2)
  df$Q_1[i+1] <- df$Q[i]
  df$Q_2[i+1] <- df$Q_1[i]
  df$Q_3[i+1] <- df$Q_2[i]
  
  #Wind speed now and the past three hours
  df$Ws[i] <- round(temp$Ws[i],2)
  df$Ws_1[i+1] <- df$Ws[i]
  df$Ws_2[i+1] <- df$Ws_1[i]
  df$Ws_3[i+1] <- df$Ws_2[i]
  
  df[is.na(df)] <- 0
}

#Normalise Data
normalise <- function(x){(x-min(x))/(max(x)-min(x))}
df_scaled <- apply(df[,-1], 2, normalise)
dated_scaled <- data.frame(df_scaled)
#dated_scaled <- dated_scaled[c(17,1:16)]
#class(dated_scaled)

#TRAINING AND TEST SETS
train = df[1:(24*24),]
test = df[(24*24+1):(24*30),]
  #SCALED
train_scaled = dated_scaled[1:(24*24),]
test_scaled = dated_scaled[(24*24+1):(24*30),]

#Formula
n <- colnames(train)
f = as.formula(paste("Tin ~", paste(n[!n %in% "Tin"], collapse = " + ")))
  #SCALED
f_scaled = as.formula(paste("Tin ~", paste(n[!n %in% c("Tin","DateTime")], collapse = " + ")))

##############NEURAL NETWORK
nn <- neuralnet(f_scaled, data = train_scaled, linear.output = TRUE)
nn_predicted <- compute(nn, test_scaled[,-1])
final_pred <- nn_predicted$net.result * (max(df$Tin) - min(df$Tin))+min(df$Tin)
actual <- test_scaled[,1] *  (max(df$Tin) - min(df$Tin))+min(df$Tin)
test$Tin_pred <- as.numeric(round(final_pred,2))
result <- data.frame(test[,c(1,2,18)])

MAE = mean(abs(final_pred-actual))
MAXAE = max(abs(final_pred-actual))
MSE = mean((final_pred - actual)^2)
RMSE = MSE^0.5

nn_results <- c(MAE, MAXAE, MSE, RMSE)

head(result)

png('nn_scatterplot.png')
ggplot(result, aes(x=actual, y=final_pred)) + geom_point() + stat_smooth() + 
  xlab('Actual') + ylab('NN_Predicted') + ggtitle('NEURAL NETWORK')
dev.off()
png('nn_.png')
ggplot(result, aes(x=DateTime, y=result)) + geom_line() + xlab('Actual') + ylab('NN_Predicted') + ggtitle('NEURAL NETWORK')
autoplot(ts(cbind(final_pred, actual, Facet=T)))
#plot(nn)

meltresult <- melt(result,id="DateTime")
ggplot(meltresult,aes(x=DateTime,y=value,colour=variable,group=variable)) + geom_line()


#LINEAR REGRESSION
lmfit <- lm(f, data=train)
lmpr <- predict(lmfit,newdata = data.frame(test[,-2]))
#This is for Inverse Normalisation
#final_pred <- lmpr * (max(df$Tin) - min(df$Tin))+min(df$Tin)
#actual <- test[,2] *  (max(df$Tin) - min(df$Tin))+min(df$Tin)
final_pred <- lmpr; actual <- test[,2]
MAE = mean(abs(final_pred-actual))
MAXAE = max(abs(final_pred-actual))
MSE = mean((final_pred- actual)^2)
RMSE = MSE^0.5

arx_results <- c(MAE, MAXAE, MSE, RMSE) 

png('arx_scatterplot.png')
ggplot(result, aes(x=actual, y=final_pred)) + geom_point() + stat_smooth() + 
  xlab('Actual') + ylab('ARX_Predicted') + ggtitle('ARX MODEL')
dev.off()

AIC(lmfit)
summary(lmfit)


#SUPPORT VECTOR MACHINE REGRESSION (SVR)
  #NON-SCALED
svm_fit <- svm(f, train, type='eps-regression')
svm_pr <- predict(svm_fit,newdata = data.frame(test[,-2]))
final_pred <- svm_pr * (max(df$Tin) - min(df$Tin))+min(df$Tin)
actual <- test[,2] *  (max(df$Tin) - min(df$Tin))+min(df$Tin)
MAE = mean(abs(final_pred-actual))
MAXAE = max(abs(final_pred-actual))
MSE = mean((final_pred- actual)^2)
RMSE = MSE^0.5

svm_ns_results <- c(MAE, MAXAE, MSE, RMSE) 
  
  #SCALED
svm_fit <- svm(f_scaled, train_scaled, type='eps-regression')
svm_pr <- predict(svm_fit,newdata = data.frame(test_scaled[,-1]))
final_pred <- svm_pr * (max(df$Tin) - min(df$Tin))+min(df$Tin)
actual <- test_scaled[,1] *  (max(df$Tin) - min(df$Tin))+min(df$Tin)
MAE = mean(abs(final_pred-actual))
MAXAE = max(abs(final_pred-actual))
MSE = mean((final_pred- actual)^2)
RMSE = MSE^0.5

svm_sc_results <- c(MAE, MAXAE, MSE, RMSE) 

autoplot(ts(cbind(final_pred, actual)))

head(cbind(final_pred, actual))

#RANDOM FOREST PREDICTION
  #NON-SCALED
rffit <- randomForest(f, data=train, ntree=500)
rfpr <- predict(rffit, newdata = data.frame(test[,-2]))
final_pred <- rfpr; actual <- test[,2]
MAE = mean(abs(final_pred-actual))
MAXAE = max(abs(final_pred-actual))
MSE = mean((final_pred- actual)^2)
RMSE = MSE^0.5

rf_ns_results <- c(MAE, MAXAE, MSE, RMSE) 

  #SCALED
rffit <- randomForest(f_scaled, data=train_scaled, ntree=500)
rfpr <- predict(rffit, newdata = data.frame(test_scaled[,-1]))
final_pred <- rfpr * (max(df$Tin) - min(df$Tin))+min(df$Tin)
actual <- test_scaled[,1] *  (max(df$Tin) - min(df$Tin))+min(df$Tin)
MAE = mean(abs(final_pred-actual))
MAXAE = max(abs(final_pred-actual))
MSE = mean((final_pred- actual)^2)
RMSE = MSE^0.5

rf_sc_results <- c(MAE, MAXAE, MSE, RMSE) 

########### SAVING RESULTs
results <- data.frame(row.names = c('MAE', 'MAXAE', 'MSE', 'RMSE'),
                      NN = nn_results, ARX = arx_results, 
                      SVM_NS = svm_ns_results, SVM_SC = svm_sc_results,
                      RF_NS = rf_ns_results, RF_SC = rf_sc_results)

write.csv(results, 'Results.csv')

head(result)

ggplot(result, aes(x=actual, y=final_pred)) + geom_point() + stat_smooth()
autoplot(ts(cbind(final_pred, actual)))
plot(nn)
summary(nn)
str(nn_predicted)
head(test[,1])







############## LINEAR REGRESSION ARX
lmfit <- lm(f, train)
test$lm_predict <- predict(lmfit, test[,-1])

final_predict <- test$lm_predict * (max(df$Tin)-min(df$Tin)) + min(df$Tin) 
test_values <- test[,1] * (max(df$Tin)-min(df$Tin)) + min(df$Tin)
mean(abs(final_predict - test_values))

test$upper <- test$lm_predict + sd(test$lm_predict)*1.96/72^0.5
test$lower <- test$lm_predict - sd(test$lm_predict)*1.96/72^0.5
test <- test[,c(1:2,14:16,3:13)]
head(test)
summary(lmfit)
lmfit$coefficients
AIC(lmfit)
mean(abs(test$lm_predict - test[,2]))

res <- residuals(lmfit)
res <- data.frame(res)
head(res)

ggplot(test) + geom_line(aes(x=DateTime, y=test[,2])) + lines(test[,2])
autoplot(ts(cbind(test$upper, test$lower))) + autolayer(ts(test[,2]), series='Actual')

ggplot(res, aes(res)) + geom_histogram(fill='blue', alpha=0.5)
plot(lmfit)

#MEAN SQUARED ERROR
mse <- mean((test$lm_predict - test[,2])^2) 
mse
#ROOT MEAN SQUARE
rms <- mse^0.5
rms


