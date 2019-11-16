library(readxl)
library(timeSeries)
library(forecast)
library(ie2misc)
library(fpp)
library(smooth)  ### for error checking 
library(dummies)


#### DATA DRIVEN TECH ######

coke <- read_xlsx("C://Users//Sanchi//Desktop//Assignment//Complete//R//Forecasting//CocaCola_Sales_Rawdata.xlsx")
plot(coke$`Price In India`,type = "o")

### converting data into time series
cokets <- ts(data = coke$`Price In India`,frequency = 4)
View(cokets)

## train and test data 
#set.seed(222)
#sample_data <- sample(nrow(cokets),(nrow(cokets)*.9),replace = FALSE)

train <- cokets[1:16]
test <- cokets[17:20]

train1 <- ts(data = train,frequency = 4)
test1 <- ts(data=test,frequency = 4)
plot(cokets)


#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter


#simple expo smoothing

simpelexpo <- HoltWinters(train1,alpha = .2,beta = F,gamma = F)
simpelexpo
simplepredict <- as.data.frame(predict(simpelexpo,n.ahead = 4))
simplepredict$fit
plot(forecast(simpelexpo,h=4))
### by looking plot we can say that there is no pattern 

a <- MAPE(simplepredict$fit,test1)*100
b <- MAPE(test1,simplepredict$fit)*100

##alpha=.02 and beta =.01 and gamma = f 
##assume that  there is level and trend is present 
## double expo or holts 

double <- HoltWinters(train1,alpha = .2,beta=.1,gamma = F)
double
double$fitted
doublepredict <- as.data.frame(predict(double,n.ahead = 4))
doublepredict
plot(forecast(double,h=4))
### by looking plot we can say that there is no pattern 

c <- MAPE(test1,doublepredict$fit)*100
d <- MAPE(doublepredict$fit,test1)*100


### with alpha = 0.2, beta = 0.1, gamma = 0.1 
holts <- HoltWinters(train1,alpha = .2,beta=.1,gamma = .1)
holtspredict <- as.data.frame(predict(holts,n.ahead =4))
holtspredict
plot(forecast(holts,h=4))
### by looking plot we can say that there is a pattern 
e <- MAPE(test1,holtspredict$fit)*100
f <- MAPE(holtspredict$fit,test1)*100



### without optimum value 
first <- HoltWinters(train1,beta = F,gamma = F)
firstprediict <- as.data.frame(predict(first,n.ahead = 4))
firstprediict
plot(forecast(first,h=4))
### by looking plot we can say that there is a pattern 
g <- MAPE(test1,firstprediict$fit)*100
h <- MAPE(firstprediict$fit,test1)*100

second <- HoltWinters(train1,gamma = F)
secondpredict=as.data.frame(predict(second,n.ahead = 4))
secondpredict
plot(forecast(second,h=4))
### by looking plot we can say that there is a pattern 
i <- MAPE(test1,secondpredict$fit)*100
j <- MAPE(secondpredict$fit,test1)*100


third <- HoltWinters(train1)
thirdpredict=as.data.frame(predict(third,n.ahead = 4))
thirdpredict
plot(forecast(third))
k <- MAPE(test1,thirdpredict$fit)*100
l <- MAPE(thirdpredict$fit,test1)*100

mapevalue <- data.frame(c("a","b","c","d","e","f","g","h","i","j","k","l"),c(a,b,c,d,e,f,g,h,i,j,k,l))
colnames(mapevalue) <- c("Mape","Values")
View(mapevalue)



#from the list we can say that third model is very good bcz it has least mape value 

## so  now we will build model for third 
newmodel <- HoltWinters(cokets)
newmodel
newmodelpredict <- as.data.frame(predict(newmodel,n.ahead = 4))
newmodelpredict
plot(forecast(newmodel))
###########################################################################



############## USING ses,holt,hw functions ##########################

four <- HoltWinters(train1,alpha = .2)
four
fourpredict <- as.data.frame(predict(four,n.ahead = 4))
fourpredict
plot(forecast(four))
m <- MAPE(test1,fourpredict$fit)*100
n <- MAPE(fourpredict$fit,test1)*100


fifth <- HoltWinters(train1,alpha = .2,beta = .1)
fifth
fifthpredict <- as.data.frame(predict(fifth,n.ahead = 4))
fifthpredict
plot(forecast(fifth))
o <- MAPE(test1,fifthpredict$fit)*100
p <- MAPE(fifthpredict$fit,test1)*100



six <- HoltWinters(train1,alpha = .2,beta = .1,gamma = .1)
six
sixpredict <- as.data.frame(predict(six,n.ahead = 4))
sixpredict
plot(forecast(six))
q <- MAPE(test1,sixpredict$fit)*100
r <- MAPE(sixpredict$fit,test1)*100


# With out optimum values 
seven <- HoltWinters(train1,alpha = NULL)
seven
sevenpredict <- as.data.frame(predict(seven,n.ahead = 4))
sevenpredict
plot(forecast(seven))
s <- MAPE(test1,sevenpredict$fit)*100
t <- MAPE(sevenpredict$fit,test1)*100



eight <- HoltWinters(train1,alpha = NULL,beta = NULL)
  eight
  eightpredict <- as.data.frame(predict(eight,n.ahead = 4))
  eightpredict
plot(forecast(eight))
u <- MAPE(test1,eightpredict$fit)*100
v <- MAPE(eightpredict$fit,test1)*100



nine <- HoltWinters(train1,alpha = NULL,beta = NULL,gamma = NULL)
nine
ninepredict <- as.data.frame(predict(nine,n.ahead = 4))
ninepredict
plot(forecast(nine))
w <- MAPE(test1,ninepredict$fit)*100
x <- MAPE(ninepredict$fit,test1)*100





finalresult <- data.frame(c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t"
,"u","v","w","x"),c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x))
colnames(finalresult) <- c("Mape","Values")
View(finalresult)



##fifth model has min mape value so we will take this for prediction

finalmodel <- HoltWinters(cokets,alpha = .2,beta = .1)
finalmodel
finalpredict <- as.data.frame(predict(finalmodel,n.ahead = 4))
finalpredict
plot(finalmodel)




######  MODEL BASED TECH ############

coke$Quarter


dummy1 <- as.data.frame(dummy(coke$Quarter))
### here i have crated 42 dummy variable 
coke <- cbind(coke,dummy1)
colnames(coke)
coke["t"] <- 1:42
coke$t
class(coke)
coke["logsales"] <- log(coke$Sales)
coke$logsales
coke["tsquare"] <- coke$t*coke$t
coke$tsquare
train2 <- coke[1:38,]
test2 <- coke[39:42,]
class(train2)
class(test2)


#####linear model########
linearmodel <- lm(Sales~t,data = train2)
summary(linearmodel)
linearpredict <- data.frame(predict(linearmodel,interval = "predict",newdata = test2))
View(linearpredict)
###rmse_value <- rmse(linearpredict$fit,test2$Sales)
###rmse_value
test2$Sales
linearpredict$fit
error=linearpredict$fit-test2$Sales
rmse0 <- sqrt(mean((error)^2))
rmse0
m0 <- MAPE(test2$Sales,linearpredict$fit)*100
#########Exponential######

expomodel <- lm(logsales~t,data = train2)
summary(expomodel)
expopredict <- as.data.frame(predict(expomodel,interval = "predict",newdata = test2))
expopredict
error1 <- expopredict$fit-test2$Sales
error1
rmse1 <- sqrt(mean((error1)^2))
rmse1
m1 <- MAPE(test2$Sales,expopredict$fit)*100
######################### Quadratic ####################################
quarmodel <- lm(Sales~t+tsquare,data = train2)
summary(quarmodel)
quarpredict <- data.frame(predict(quarmodel,interval = "predict",newdata = test2))
quarpredict
error2 <- quarpredict$fit-test2$Sales
rmse2 <- sqrt(mean((error2)^2))
rmse2
m2 <- MAPE(test2$Sales,quarpredict$fit)*100

######################### Additive Seasonality #########################
add_model<-lm(Sales~train2$`Quarter)Q1_86`+train2$`Quarter)Q1_87`+train2$`Quarter)Q1_88`+train2$`Quarter)Q1_89`
              +train2$`Quarter)Q1_90`+train2$`Quarter)Q1_91`+train2$`Quarter)Q1_92`+train2$`Quarter)Q1_93`+train2$`Quarter)Q1_94`+
                train2$`Quarter)Q1_95`+train2$`Quarter)Q1_96`+train2$`Quarter)Q2_86`+train2$`Quarter)Q2_87`+train2$`Quarter)Q2_88`+
                train2$`Quarter)Q2_89`+train2$`Quarter)Q2_90`+train2$`Quarter)Q2_91`+train2$`Quarter)Q2_92`
              +train2$`Quarter)Q2_93`+train2$`Quarter)Q2_94`+train2$`Quarter)Q2_95`+train2$`Quarter)Q2_96`+train2$`Quarter)Q3_86`+train2$`Quarter)Q3_87`
              +train2$`Quarter)Q3_88`+train2$`Quarter)Q3_89`+train2$`Quarter)Q3_90`+train2$`Quarter)Q3_91`+train2$`Quarter)Q3_92`
              +train2$`Quarter)Q3_93`+train2$`Quarter)Q3_94`+train2$`Quarter)Q3_95`+train2$`Quarter)Q4_86`+train2$`Quarter)Q4_87`+train2$`Quarter)Q4_88`+train2$`Quarter)Q4_89`+train2$`Quarter)Q4_90`
              +train2$`Quarter)Q4_91`+train2$`Quarter)Q4_92`+train2$`Quarter)Q4_93`+train2$`Quarter)Q4_94`+train2$`Quarter)Q4_95`,data=train2)
summary(add_model)
add_pred<-data.frame(predict(add_model,interval='predict',newdata = test2))
rmse3<-sqrt(mean((add_pred$fit-test2$Sales)^2,na.rm = T))
rmse3

m3 <- MAPE(test2$Sales,add_pred$fit)

######################## Additive Seasonality with Linear #################
addlinear_model<-lm(Sales~train2$`Quarter)Q1_86`+train2$`Quarter)Q1_87`+train2$`Quarter)Q1_88`+train2$`Quarter)Q1_89`
              +train2$`Quarter)Q1_90`+train2$`Quarter)Q1_91`+train2$`Quarter)Q1_92`+train2$`Quarter)Q1_93`+train2$`Quarter)Q1_94`+
                train2$`Quarter)Q1_95`+train2$`Quarter)Q1_96`+train2$`Quarter)Q2_86`+train2$`Quarter)Q2_87`+train2$`Quarter)Q2_88`+
                train2$`Quarter)Q2_89`+train2$`Quarter)Q2_90`+train2$`Quarter)Q2_91`+train2$`Quarter)Q2_92`
              +train2$`Quarter)Q2_93`+train2$`Quarter)Q2_94`+train2$`Quarter)Q2_95`+train2$`Quarter)Q2_96`+train2$`Quarter)Q3_86`+train2$`Quarter)Q3_87`
              +train2$`Quarter)Q3_88`+train2$`Quarter)Q3_89`+train2$`Quarter)Q3_90`+train2$`Quarter)Q3_91`+train2$`Quarter)Q3_92`
              +train2$`Quarter)Q3_93`+train2$`Quarter)Q3_94`+train2$`Quarter)Q3_95`+train2$`Quarter)Q4_86`+train2$`Quarter)Q4_87`+train2$`Quarter)Q4_88`+train2$`Quarter)Q4_89`+train2$`Quarter)Q4_90`
              +train2$`Quarter)Q4_91`+train2$`Quarter)Q4_92`+train2$`Quarter)Q4_93`+train2$`Quarter)Q4_94`+train2$`Quarter)Q4_95`+train2$t,data=train2)
summary(addlinear_model)
predictlinear <- data.frame(predict(addlinear_model,interval = "predict",newdata = test2)) 
rmse4<-sqrt(mean((predictlinear$fit-test2$Sales)^2,na.rm = T))
rmse4
m4 <- MAPE(test2$Sales,predictlinear$fit)


######################## Additive Seasonality with Quadratic #################

addquar_model<-lm(Sales~train2$`Quarter)Q1_86`+train2$`Quarter)Q1_87`+train2$`Quarter)Q1_88`+train2$`Quarter)Q1_89`
                    +train2$`Quarter)Q1_90`+train2$`Quarter)Q1_91`+train2$`Quarter)Q1_92`+train2$`Quarter)Q1_93`+train2$`Quarter)Q1_94`+
                      train2$`Quarter)Q1_95`+train2$`Quarter)Q1_96`+train2$`Quarter)Q2_86`+train2$`Quarter)Q2_87`+train2$`Quarter)Q2_88`+
                      train2$`Quarter)Q2_89`+train2$`Quarter)Q2_90`+train2$`Quarter)Q2_91`+train2$`Quarter)Q2_92`
                    +train2$`Quarter)Q2_93`+train2$`Quarter)Q2_94`+train2$`Quarter)Q2_95`+train2$`Quarter)Q2_96`+train2$`Quarter)Q3_86`+train2$`Quarter)Q3_87`
                    +train2$`Quarter)Q3_88`+train2$`Quarter)Q3_89`+train2$`Quarter)Q3_90`+train2$`Quarter)Q3_91`+train2$`Quarter)Q3_92`
                    +train2$`Quarter)Q3_93`+train2$`Quarter)Q3_94`+train2$`Quarter)Q3_95`+train2$`Quarter)Q4_86`+train2$`Quarter)Q4_87`+train2$`Quarter)Q4_88`+train2$`Quarter)Q4_89`+train2$`Quarter)Q4_90`
                    +train2$`Quarter)Q4_91`+train2$`Quarter)Q4_92`+train2$`Quarter)Q4_93`+train2$`Quarter)Q4_94`+train2$`Quarter)Q4_95`+train2$t+train2$tsquare,data=train2)
summary(addquar_model)
predictquar <- data.frame(predict(addquar_model,interval = "predict",newdata = test2)) 
rmse5<-sqrt(mean((predictquar$fit-test2$Sales)^2,na.rm = T))
rmse5

m5 <- MAPE(test2$Sales,predictquar$fit)

######################## Multiplicative Seasonality #########################


multi_model<-lm(train2$logsales~train2$`Quarter)Q1_86`+train2$`Quarter)Q1_87`+train2$`Quarter)Q1_88`+train2$`Quarter)Q1_89`
                  +train2$`Quarter)Q1_90`+train2$`Quarter)Q1_91`+train2$`Quarter)Q1_92`+train2$`Quarter)Q1_93`+train2$`Quarter)Q1_94`+
                    train2$`Quarter)Q1_95`+train2$`Quarter)Q1_96`+train2$`Quarter)Q2_86`+train2$`Quarter)Q2_87`+train2$`Quarter)Q2_88`+
                    train2$`Quarter)Q2_89`+train2$`Quarter)Q2_90`+train2$`Quarter)Q2_91`+train2$`Quarter)Q2_92`
                  +train2$`Quarter)Q2_93`+train2$`Quarter)Q2_94`+train2$`Quarter)Q2_95`+train2$`Quarter)Q2_96`+train2$`Quarter)Q3_86`+train2$`Quarter)Q3_87`
                  +train2$`Quarter)Q3_88`+train2$`Quarter)Q3_89`+train2$`Quarter)Q3_90`+train2$`Quarter)Q3_91`+train2$`Quarter)Q3_92`
                  +train2$`Quarter)Q3_93`+train2$`Quarter)Q3_94`+train2$`Quarter)Q3_95`+train2$`Quarter)Q4_86`+train2$`Quarter)Q4_87`+train2$`Quarter)Q4_88`+train2$`Quarter)Q4_89`+train2$`Quarter)Q4_90`
                  +train2$`Quarter)Q4_91`+train2$`Quarter)Q4_92`+train2$`Quarter)Q4_93`+train2$`Quarter)Q4_94`+train2$`Quarter)Q4_95`,data=train2)
summary(multi_model)
predictmulti <- data.frame(predict(multi_model,interval = "predict",newdata = test2)) 
rmse6<-sqrt(mean((predictmulti$fit-test2$Sales)^2,na.rm = T))
rmse6

m4 <- MAPE(test2$Sales,predictmulti$fit)

######################## Multiplicative Seasonality Linear trend ##########################
multi_t_model<-lm(train2$logsales~train2$`Quarter)Q1_86`+train2$`Quarter)Q1_87`+train2$`Quarter)Q1_88`+train2$`Quarter)Q1_89`
                +train2$`Quarter)Q1_90`+train2$`Quarter)Q1_91`+train2$`Quarter)Q1_92`+train2$`Quarter)Q1_93`+train2$`Quarter)Q1_94`+
                  train2$`Quarter)Q1_95`+train2$`Quarter)Q1_96`+train2$`Quarter)Q2_86`+train2$`Quarter)Q2_87`+train2$`Quarter)Q2_88`+
                  train2$`Quarter)Q2_89`+train2$`Quarter)Q2_90`+train2$`Quarter)Q2_91`+train2$`Quarter)Q2_92`
                +train2$`Quarter)Q2_93`+train2$`Quarter)Q2_94`+train2$`Quarter)Q2_95`+train2$`Quarter)Q2_96`+train2$`Quarter)Q3_86`+train2$`Quarter)Q3_87`
                +train2$`Quarter)Q3_88`+train2$`Quarter)Q3_89`+train2$`Quarter)Q3_90`+train2$`Quarter)Q3_91`+train2$`Quarter)Q3_92`
                +train2$`Quarter)Q3_93`+train2$`Quarter)Q3_94`+train2$`Quarter)Q3_95`+train2$`Quarter)Q4_86`+train2$`Quarter)Q4_87`+train2$`Quarter)Q4_88`+train2$`Quarter)Q4_89`+train2$`Quarter)Q4_90`
                +train2$`Quarter)Q4_91`+train2$`Quarter)Q4_92`+train2$`Quarter)Q4_93`+train2$`Quarter)Q4_94`+train2$`Quarter)Q4_95`+train2$t,data=train2)
summary(multi_t_model)
predictmulti_trend <- data.frame(predict(multi_t_model,interval = "predict",newdata = test2)) 
rmse7<-sqrt(mean((predictmulti_trend$fit-test2$Sales)^2,na.rm = T))
rmse7
m4 <- MAPE(test2$Sales,predictmulti_trend$fit)


rmsetable <- data.frame(c("rmse0","rmse1","rmse2","rmse3","rmse4","rmse5","rmse6",
                          "rmse7"),c(rmse0,rmse1,rmse2,rmse3,rmse4,rmse5,rmse6,rmse7))
colnames(rmsetable) <- c("model","rmse")
View(rmsetable)

## rmse2 has least value 
# Quadratic give the least rmse value 
## we forecast value by this model
model_final <- lm(Sales~t+tsquare,data = coke)
summary(model_final)
model_final$residuals
resid<- residuals(model_final)

acf(resid,lag.max = 10)
# many significant lags 



#### now try with Arima Model 
## AR  I   MA
##Autoregressive{p} intregity{d} Moving Average{q} 
plot(cokets,type="o")
##ts plot
class(cokets)
start(cokets)
end(cokets)
abline(reg = lm(cokets~time(cokets)))
##mean  or fit a line

plot(aggregate(cokets,FUN=mean))
###increasing trend 

frequency(cokets)
##intervals

summary(cokets)

cycle(cokets)

boxplot(cokets~cycle(cokets))
##in 2nd quater max sales then quater3 then q4 then q1 
##no seasonaliy 


###convert it into stationary bcz mean is increasing variance is also not equal


plot((log(cokets)))

###now variance is equal

plot(diff(log(cokets)))
###now mean & variance is constant  its mean   ts is stationary 

acf(cokets)

acf(diff(log(cokets)))
### determines the value of q
## we will take 0 line bcz 1 st line is insignificant 
##q =0

Pacf(diff(log(cokets)))
## determines the value of p


##d= differential(diff)=1
## bcz i done diff only one time and got constant mean and variance 

###apply arima model
##c(p,d,q)
model <- arima(log(cokets),c(0,1,0),seasonal = list(order=c(0,1,0)))
model
predic <- predict(model,n.ahead = 4)
predic
predic$pred

##these value is in log form so for conversion use e value that is 2.718
predict1 <- 2.718^predic$pred
predict1
ts.plot(cokets,predict1,log="y",lty=c(1,3))
plot(forecast(model))

###testing 
testarima <- ts(cokets,frequency = 4,start =c(1,1),end = c(10,4) )
testarima
###test with 40 data points
modelchecking <- arima(log(testarima),c(0,1,0),seasonal =list(order=c(0,1,0)) )
modelchecking
pred <- predict(modelchecking,n.ahead = 2)
pred
prediction<- pred$pred^2.718
prediction
orignal <- tail(cokets,2)
orignal
error5 <- prediction-orignal
### there is to much diffrence
rmse10 <- sqrt(mean((error5)^2))
rmse10
MAPE(orignal,prediction)*100




#### so we have done with all model 
## data driven , model based and arima 
## we use 42 dummy variable 
## finally we will take least mape value model for the forecasting 
### so according to me we will take third model for the forecasting 

