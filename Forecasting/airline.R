library(forecast)
library(smooth)
library(fpp)
library(timeSeries)
library(readxl)
library(Amelia)
library(psych)
library(dummies)
library(readxl)
airline <- read_xlsx("C://Users//Sanchi//Desktop//Assignment//Complete//R//Forecasting//Airlines+Data.xlsx")
missmap(airline)
##0%% missing value 
View(airline)
class(airline)
airline1 <- ts(airline$Passengers)
class(airline1)
plot(airline1,type="o")
abline(reg = lm(airline1~time(airline1)))
plot(log(airline1),type="o")
plot(diff(log(airline1)),type="o")
plot(diff(diff(log(airline1))),type="o")


t <- (1:96)
tsqaure <- (t*t)
log_passenger=(log(airline$Passengers))
dummy2 <- as.data.frame(outer(rep(month.abb,length=96),month.abb,"==")+0)
colnames(dummy2) <- month.abb
airline <- cbind(airline,t,tsqaure,log_passenger,dummy2)

###spiliting the data
train <- airline[1:84,]
test <- airline[85:96,]




model1 <- lm(Passengers ~ t, data= train)
summary(model1)           
pred1 <- predict(model1, test)
error1 <- test$Passengers-pred1

lm_rmse <- sqrt(sum(error1^2)/nrow(test))  ## rmse = 53.199

############# EXPONENTIAL MODEL ##############

model2 <- lm(log_passenger ~ t, data = train)
summary(model2)   ## adj r sq = 0.8218
pred2 <- exp(predict(model2, test))
error2 <- test$Passengers - pred2

exp_rmse <- sqrt(sum(error2^2)/nrow(test))  ## rmse = 46.057

############# QUADRATIC MODEL #############

model3 <- lm(Passengers ~ t+tsqaure, data= train)
summary(model3) ## adj r sq = 0.7912
pred3 <- predict(model3, test)
error3 <- test$Passengers-pred3

quad_rmse <- sqrt(sum(error3^2)/nrow(test))  ## rmse = 48.051

############# ADDITIVE SEASONALITY #########

model4 <- lm(Passengers~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(model4)   ## adj r sq = 0.04015
pred4 <- predict(model4, test)
error4 <- test$Passengers - pred4

add_rmse <- sqrt(sum(error4^2)/nrow(test))   ## rmse = 132.819

############# MULTIPLICATIVE SEASONALITY ############

model5 <- lm(log_passenger ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(model5)   ## adj r sq = 0.02568
pred5 <- exp(predict(model5, test))
error5 <- test$Passengers - pred5

mul_rmse <- sqrt(sum(error5^2)/nrow(test))   ## rmse = 140.063

############# ADDITIVE WITH QUADRATIC ############

model6 <- lm(Passengers ~ t+tsqaure+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(model6)  ## adj r sq = 0.9524
pred6 <- predict(model6, test)
error6 <- test$Passengers - pred6

add_quad_rmse <- sqrt(sum(error6^2)/nrow(test))  ## rmse = 26.360

rmse_table <- as.data.frame(rbind(lm_rmse, exp_rmse, quad_rmse, add_rmse, mul_rmse, add_quad_rmse))
View(rmse_table)



## exp model is good


## ARIMA model

start(airline1)
end(airline1)
frequency(airline1)
train1 <- ts(airline1[1:84],frequency = 12)
test1 <- ts(airline1[85:96],frequency = 12)
frequency(airline1)
cycle(airline1)
boxplot(airline1~cycle(airline1))

acf(airline1)
acf(diff(diff(log(airline1))))
#q=1
pacf(diff(diff(log(airline1))))
#p=1
#d=2



model1 <- arima(log(train1),c(1,2,1),seasonal = list(order=c(1,2,1)))
model1
predic <- predict(model1,n.ahead = 12)
predic
predic$pred
##these value is in log form so for conversion use e value that is 2.718
predict1 <- 2.718^predic$pred
predict1
ts.plot(train1,predict1,log="y",lty=c(1,3))
plot(forecast(model1))



###testing 
start(airline1)
end(airline1)
testarima <- ts(airline1,frequency = 12,start =c(1,1),end = c(96,11) )
testarima
###test with 40 data points
modelchecking <- arima(log(testarima),c(1,2,1),seasonal =list(order=c(1,2,1)) )
modelchecking
pred <- predict(modelchecking,n.ahead = 1)
pred
prediction<- pred$pred^2.718
prediction
orignal <- tail(plastics,1)
orignal
### there is to much diffrence

