library(Amelia)
library(psych)
library(caret)
library(gmodels)
library(ggplot2)
library(neuralnet)
library(nnet)
library(dummies)
library(devtools)##for github
library(ie2misc) #error caculation
#library(smooth)#error caculation### forecasting
#library(greybox)#error calculation ## forecasting 
##library(hydroGOF)## its for time series

company <- read.csv("C://Users//Acer//Desktop//Assignment//pending//neural network//50_Startups.csv")
View(company)

####EDA###

describe(company)
## convert state iinto dummy
d1 <- as.data.frame(dummy(company$State))
company <-cbind(company,d1)

##value is to mch high so we will do normalization

normal <- as.data.frame(scale(company[,c(-4)]))
state<- (company[,c(4)])
normal <- cbind(state,normal)
str(normal)
describe(normal)
## we got 1st 2nd 3rd 4th BM
summary(normal)

summary(normal$state)


missmap(company)
##0% missing value 
missmap(normal)
##0% missing value 

### Outlier  checking
 

c <- ggplot(data = normal,aes(x=normal$R.D.Spend,y=normal$R.D.Spend,group=1))+geom_boxplot()

e <-c+theme(panel.background = element_rect(fill = "skyblue"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "REd"))
g

#### so now find that there is no outlier in R.D.Spend 

h <- ggplot(data=normal,aes(x=normal$Administration,y=normal$Administration,group=2))+geom_boxplot()
i <- h+theme(panel.background = element_rect(fill = "light green"))
j <- i+theme(plot.title = element_text(hjust=.05,face="bold"))
j

#### so now find that there is no outlier in Administration

h <- ggplot(data=normal,aes(x=normal$Marketing.Spend,y=normal$Marketing.Spend,group=3))+geom_boxplot()
i <- h+theme(panel.background = element_rect(fill = "purple"))
j <- i+theme(plot.title = element_text(hjust=.05,face="bold"))
j


#### so now find that there is no outlier in Marketing.Spend


h <- ggplot(data=normal,aes(x=normal$Profit,y=normal$Profit,group=4))+geom_boxplot()
i <- h+theme(panel.background = element_rect(fill = "sky blue"))
j <- i+theme(plot.title = element_text(hjust=.05,face="bold"))
j

## so we find one outlier we will replace it with median
##change the outlier value is totaaly depend on the client 

k <- which(normal$Profit < -2)
boxplot(k)
##in row 50 i found a outlier
median(normal$Profit)
mean(normal$Profit)
levels(normal$Profit) <- c(levels(normal$Profit),-0.1000951)
normal$Profit[normal$Profit < -2] <- -0.1000951

h <- ggplot(data=normal,aes(x=normal$Profit,y=normal$Profit,group=4))+geom_boxplot()
i <- h+theme(panel.background = element_rect(fill = "sky blue"))
j <- i+theme(plot.title = element_text(hjust=.05,face="bold"))
j

#### so now find that there is no outlier in Profit

##visualization 
l <- ggplot(data=company,aes(x=company$R.D.Spend,fill=company$State))+geom_histogram()
m <- l+theme(panel.background = element_rect(fill = "white"))
n <- m+theme(plot.title = element_text(hjust=.05,face="bold"))
n

## from this we can say that maximum R.D. spend are rom florida state 
##min is from all state 
##highest from newyork and california 


l <- ggplot(data=company,aes(x=company$R.D.Spend,fill=company$State))+geom_bar()
m <- l+theme(panel.background = element_rect(fill = "black"))
n <- m+theme(plot.title = element_text(hjust=.05,face="bold"))
n
##here we can say that max is from newyork

l <- ggplot(data=company,aes(x=company$Administration,fill=company$State))+geom_histogram()
m <- l+theme(panel.background = element_rect())
n <- m+theme(plot.title = element_text(hjust=.05,face="bold"))
n
## administration in all state is average max administration is from florida

l <- ggplot(data=company,aes(x=company$Marketing.Spend,fill=company$State))+geom_histogram()
m <- l+theme(panel.background = element_rect())
n <- m+theme(plot.title = element_text(hjust=.05,face="bold"))
n

##max marketing spend is from newyork 
##floida dont have least 
#florida is average 

l <- ggplot(data=company,aes(x=company$Profit,fill=company$State))+geom_histogram()
m <- l+theme(panel.background = element_rect())
n <- m+theme(plot.title = element_text(hjust=.05,face="bold"))
n
##california has least and max profit


company<- company[,c(-4)]
normal<- normal[,c(-1)]
## now we dont have any categorical data in data set so we divide data
## i am going to use simple function for spliting 
##spliting the data in to 70% and 30%
### 70 % for the training 
#30% for the validation 
colnames(normal)[5] <-"california" 
colnames(normal)[6] <-"florida" 
colnames(normal)[7] <-"new_york" 

set.seed(222)
sample_data <- sample(nrow(normal),(nrow(normal)*.7))
train <- normal[sample_data,]
test <-  normal[-sample_data,]


###model building 
model1 <- neuralnet(train$Profit~.,data = train,hidden = 4)

model1
summary(model1)
plot(model1)
#ERROR .16
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(model1)

#pred0 <- predict(model1,test)
#error0 <- (pred0)-(test$Profit)
#sqrt(sum(error0^2)/nrow(test))
#.97
pred00 <- compute(model1,test)
pred00$net.result
pred00$neurons
#prob <- Pred00$net.result
#pred <- ifelse(prob>0.5, 1, 0)
#pred

rmse(pred0,test$Profit)
## root mean 
#.97

## mae(pred0,test$Profit) ## mean abs error 
mape(pred0,test$Profit) ## mean abs % error
##148.8001
# madstat(test$Profit)#mean abs deviation
# mean(sum(error0)^2)/nrow(test)# mean squared error 
# MPE(pred0,test$Profit)#mean percentage error 
# MAPE(pred0,test$Profit)
#mean(error0)## mean error 

model2 <- neuralnet(train$Profit~.,data = train,hidden = c(5,3,2,1))
plot(model2)
##ERROR .01
##windows()
plot.nnet(model2)
pred1 <- predict(model2,test)
error1 <- (pred1)-(test$Profit)
sqrt(sum(error1^2)/nrow(test))
#.57
rmse(pred1,test$Profit)
#.57
mape(pred1,test$Profit)
##120.88

model3 <- neuralnet(train$Profit~.,data = train,hidden = c(5,3,2))
plot(model3)
##ERROR .03
##windows()
plot.nnet(model3)
pred2 <- predict(model3,test)
error2 <- (pred2)-(test$Profit)
sqrt(sum(error2^2)/nrow(test))
#.57
rmse(pred2,test$Profit)
#1.08
mape(pred2,test$Profit)
##250.5178


##least error value best model 

## so we will conside model2 for prediction 
