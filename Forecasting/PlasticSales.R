library(readxl)
library(readr)
library(forecast)
library(smooth)
library(ie2misc)
library(timeSeries)
library(fpp)
library(dummies)
plastic <- read.csv("C://Users//Sanchi//Desktop//Assignment//Complete//R//Forecasting//PlasticSales.csv")
View(plastic)
View(plastics)
plot(plastic$Sales,type="o")
class(plastic)
plastics1 <- ts(plastic$Sales)
abline(reg = lm(plastics1~time(plastics1)))
plot(log(plastic$Sales),type="o")
plot(diff(log(plastic$Sales)),type="o")
plot(diff(diff(log(plastic$Sales))),type="o")
plot(diff(diff(diff(log(plastic$Sales)))),type="o")

### now it is stationary 
### creating month as a dummy
##dummy1=as.data.frame(dummy(plastic$Month))
dummy2 <- as.data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
colnames(dummy2) <- month.abb
plastic <- cbind(plastic,dummy2)
plastic["t"] <- 1:60
plastic["tsquare"] <- plastic$t*plastic$t
plastic["log_sales"] <- log(plastic$Sales)


### spliting the data

train <- plastic[1:48,]
test <- plastic[49:60,]


##linear model

model1 <- lm(Sales~t,data = train)
model1
summary(model1)
#plot(model1)

predict1 <- predict(model1,test)
length(predict1)
length(test$Sales)
rmse1 <- rmse(predict1,test$Sales)
rmse1

m1 <- MAPE(predict1,test$Sales)*100
m1
##expo
model2 <- lm(log_sales~ t,data = train)
summary(model2)
predict0 <- predict(model2,test)
predict2<- 2.718^predict0
## bcz it is in log 
rmse2 <- rmse(predict2,test$Sales)
rmse2
m2 <- MAPE(predict2,test$Sales)*100
m2

############# QUADRATIC MODEL #############

model3 <- lm(Sales ~ t+tsquare, data= train)
summary(model3) 
predict3 <- predict(model3, test)
rmse3 <- rmse(predict3,test$Sales)
rmse3
m3 <- MAPE(predict3,test$Sales)*100
m3

############# ADDITIVE SEASONALITY #########

model4 <- lm(Sales~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(model4)   
predict4 <- predict(model4, test)
rmse4 <-rmse(predict4,test$Sales) 
rmse4
m4 <- MAPE(predict4,test$Sales)*100
m4

############# MULTIPLICATIVE SEASONALITY ############

model5 <- lm(log_sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(model5)   
predict5 <- (predict(model5, test))
predict05 <- 2.718^predict5
rmse5 <- rmse(predict05,test$Sales)
rmse5
m5 <- MAPE(predict05,test$Sales)*100
m5

############# ADDITIVE WITH QUADRATIC ############

model6 <- lm(Sales ~ t+tsquare+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(model6)  
predict6 <- predict(model6, test)
predict6
rmse6 <- rmse(predict6,test$Sales)
rmse6
m6 <- MAPE(predict6,test$Sales)*100
m6


rmsetable <- as.data.frame(rbind(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6))
View(rmsetable)

### rmse6 is least 
## so from rmse we will take model 6
## so it mean according to model based model 6 is best 

#####data driven ###########

train1 <- ts(data = plastics1[1:48],frequency = 12)
test1 <- ts(data=plastics1[49:60],frequency = 12)
plot(plastics1)


#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter


#simple expo smoothing

simpelexpo <- HoltWinters(train1,alpha = .2,beta = F,gamma = F)
simpelexpo
simplepredict <- as.data.frame(predict(simpelexpo,n.ahead = 12))
simplepredict$fit
plot(forecast(simpelexpo,h=12))
### by looking plot we can say that there is no pattern 

a <- MAPE(simplepredict$fit,test1)*100

##alpha=.02 and beta =.01 and gamma = f 
##assume that  there is level and trend is present 
## double expo or holts 

double <- HoltWinters(train1,alpha = .2,beta=.1,gamma = F)
double
double$fitted
doublepredict <- as.data.frame(predict(double,n.ahead = 12))
doublepredict
plot(forecast(double,h=12))
### by looking plot we can say that there is no pattern 

c <- MAPE(test1,doublepredict$fit)*100
d <- MAPE(doublepredict$fit,test1)*100


### with alpha = 0.2, beta = 0.1, gamma = 0.1 
holts <- HoltWinters(train1,alpha = .2,beta=.1,gamma = .1)
holtspredict <- as.data.frame(predict(holts,n.ahead =12))
holtspredict
plot(forecast(holts,h=12))
### by looking plot we can say that there is a pattern 
e <- MAPE(test1,holtspredict$fit)*100
f <- MAPE(holtspredict$fit,test1)*100



### without optimum value 
first <- HoltWinters(train1,beta = F,gamma = F)
firstprediict <- as.data.frame(predict(first,n.ahead = 12))
firstprediict
plot(forecast(first,h=12))
### by looking plot we can say that there is a pattern 
g <- MAPE(test1,firstprediict$fit)*100
h <- MAPE(firstprediict$fit,test1)*100

second <- HoltWinters(train1,gamma = F)
secondpredict=as.data.frame(predict(second,n.ahead = 12))
secondpredict
plot(forecast(second,h=12))
### by looking plot we can say that there is a pattern 
i <- MAPE(test1,secondpredict$fit)*100
j <- MAPE(secondpredict$fit,test1)*100


third <- HoltWinters(train1)
thirdpredict=as.data.frame(predict(third,n.ahead = 12))
thirdpredict
plot(forecast(third))
k <- MAPE(test1,thirdpredict$fit)*100
l <- MAPE(thirdpredict$fit,test1)*100
###########################################################################



############## USING ses,holt,hw functions ##########################

four <- HoltWinters(train1,alpha = .2)
four
fourpredict <- as.data.frame(predict(four,n.ahead = 12))
fourpredict
plot(forecast(four))
m <- MAPE(test1,fourpredict$fit)*100
n <- MAPE(fourpredict$fit,test1)*100


fifth <- HoltWinters(train1,alpha = .2,beta = .1)
fifth
fifthpredict <- as.data.frame(predict(fifth,n.ahead = 12))
fifthpredict
plot(forecast(fifth))
o <- MAPE(test1,fifthpredict$fit)*100
p <- MAPE(fifthpredict$fit,test1)*100



six <- HoltWinters(train1,alpha = .2,beta = .1,gamma = .1)
six
sixpredict <- as.data.frame(predict(six,n.ahead = 12))
sixpredict
plot(forecast(six))
q <- MAPE(test1,sixpredict$fit)*100
r <- MAPE(sixpredict$fit,test1)*100


# With out optimum values 
seven <- HoltWinters(train1,alpha = NULL)
seven
sevenpredict <- as.data.frame(predict(seven,n.ahead = 12))
sevenpredict
plot(forecast(seven))
s <- MAPE(test1,sevenpredict$fit)*100
t <- MAPE(sevenpredict$fit,test1)*100



eight <- HoltWinters(train1,alpha = NULL,beta = NULL)
eight
eightpredict <- as.data.frame(predict(eight,n.ahead = 12))
eightpredict
plot(forecast(eight))
u <- MAPE(test1,eightpredict$fit)*100
v <- MAPE(eightpredict$fit,test1)*100



nine <- HoltWinters(train1,alpha = NULL,beta = NULL,gamma = NULL)
nine
ninepredict <- as.data.frame(predict(nine,n.ahead = 12))
ninepredict
plot(forecast(nine))
w <- MAPE(test1,ninepredict$fit)*100
x <- MAPE(ninepredict$fit,test1)*100
mape(ninepredict$fit,test1)



finalresult <- data.frame(rbind(a,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,m1,m2,m3,m4,m5,m6))
View(finalresult)
### n has least value 
## fourth model is use for forecasting

finalmodel<- HoltWinters(plastics,alpha = .2)
finalmodel
finalpredict <- as.data.frame(predict(four,n.ahead = 12))
finalpredict
plot(forecast(finalmodel))





###### arima model


##Autoregressive{p} intregity{d} Moving Average{q} 
plot(plastics,type="o")
##ts plot
start(plastics)
end(plastics)

plot(aggregate(plastics,FUN=mean))
###increasing trend 

frequency(plastics)
##intervals


cycle(plastics)

boxplot(plastics~cycle(plastics))
##in 6nd month max sales 

acf(plastics)

acf(diff(log(plastics)))
### determines the value of q
## we will take 0 line bcz 1 st line is insignificant 
##q =2

Pacf(diff(log(plastics)))
## determines the value of p=0


##d= differential(diff)=3
## bcz i done diff only 3 time and got constant mean and variance 

###apply arima model
##c(p,d,q)
model111 <- arima(log(plastics),c(0,3,2),seasonal = list(order=c(0,3,2)))
model111
predic <- predict(model111,n.ahead = 12)
predic
predic$pred

##these value is in log form so for conversion use e value that is 2.718
predict1 <- 2.718^predic$pred
predict1
ts.plot(plastics,predict1,log="y",lty=c(1,3))
plot(forecast(model111))

###testing 
testarima <- ts(plastics,frequency = 12,start =c(1,1),end = c(5,11) )
testarima
###test with 40 data points
modelchecking <- arima(log(testarima),c(0,3,2),seasonal =list(order=c(0,3,2)) )
modelchecking
pred <- predict(modelchecking,n.ahead = 1)
pred
prediction<- pred$pred^2.718
prediction
orignal <- tail(plastics,1)
orignal
### there is to much diffrence
### so we will not take this model


#### AR model
############# ARIMA MODEL ################

model <- auto.arima(train1, trace = TRUE)
model$residuals
pred <- forecast(model, h = 12)
pred$mean

plot(forecast(model))

arima_mape <- MAPE(pred$mean,test1[1:12])*100
arima_mape
## using arima method on whole data

final.model <- auto.arima(plastics, trace = TRUE)
final.model$residuals
pred <- forecast(final.model, h = 12)
pred$mean

plot(forecast(model))


### we will take AR model for forecasting 

#### so we have done with all model 
## data driven , model based and arima 
## we use 12 dummy variable 



