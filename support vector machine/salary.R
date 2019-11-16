train <- read.csv("C://Users//Acer//Desktop//Assignment//pending//support vector machine//SalaryData_Train.csv")
View(train)
test <-read.csv("C://Users//Acer//Desktop//Assignment//pending//support vector machine//SalaryData_Test.csv")
View(test)  
str(train)
str(test)

#education(3) and education num(4) are same so remove any one 
## as my domain knowledge maritalstatus5,relationship7,sex 9 is imp so i cant remove it 
train <- train[,c(-4)]
test <- test[,c(-4)]


## we build model by train data and validate by test data 
library(psych)
describe(train)
describe(test)

library(Amelia)
missmap(train)
missmap(test)
##0% missing


###  outlier checking

library(ggplot2)
c <- ggplot(data = train,aes(x=train$age,y=train$age,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill = "Purple"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

h<- train$age[train$age<76]
boxplot(h)
which(train$age>76)
median(train$age)
##change outlier with median
levels(train$age) <- c(levels(train$age),37)
train$age[train$age>=76] <- 37

c <- ggplot(data = train,aes(x=train$age,y=train$age,group=1))+geom_boxplot(col="coral")
e <- c+theme(panel.background = element_rect(fill = "Purple"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g


## now we finally got a age without any outlier

c <- ggplot(data = train,aes(x=train$workclass,y=train$workclass,group=1))+geom_col(col="gold")

e <- c+theme(panel.background = element_rect())
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
h <- g+facet_grid(.~train$Salary)
h
## from this we can say that most of the emp are working in private sec


c <- ggplot(data = train,aes(x=train$capitalgain,y=train$capitalgain,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill="skyblue"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

h<- train$capitalgain[train$capitalgain<110]
boxplot(h)
which(train$age>=110)
median(train$capitalgain)
##change outlier with median
levels(train$capitalgain) <- c(levels(train$capitalgain),0)
train$capitalgain[train$capitalgain>=110] <- 0

c <- ggplot(data = train,aes(x=train$capitalgain,y=train$capitalgain,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill="skyblue"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

## now we finally got cpital gain without any outlier


c <- ggplot(data = train,aes(x=train$capitalloss,y=train$capitalloss,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill="lightyellow"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

h <- train$capitalloss[train$capitalloss<150]
boxplot(h)
median(train$capitalloss)
levels(train$capitalloss) <- c(levels(train$capitalloss),0)
train$capitalloss[train$capitalloss>=150] <- 0

c <- ggplot(data = train,aes(x=train$capitalloss,y=train$capitalloss,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill="lightyellow"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

## now we finally got a cpitalloss without any outlier


c <- ggplot(data = train,aes(x=train$hoursperweek,y=train$hoursperweek,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill="lightyellow"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

h <- train$hoursperweek[train$hoursperweek<48]
boxplot(h)
k <- train$hoursperweek[train$hoursperweek>60]
boxplot(k)

median(train$hoursperweek)
levels(train$hoursperweek) <- c(levels(train$hoursperweek),40)
train$hoursperweek[train$hoursperweek>=48] <- 40
train$hoursperweek[train$hoursperweek<=60] <- 40

c <- ggplot(data = train,aes(x=train$hoursperweek,y=train$hoursperweek,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill="lightyellow"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g


## no outlier

c <- ggplot(data = test,aes(x=test$age,y=test$age,group=1))+geom_boxplot(col="coral")
e <- c+theme(panel.background = element_rect(fill = "Purple"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

h<- test$age[test$age<76]
boxplot(h)
which(test$age>76)
median(test$age)
##change outlier with median
levels(test$age) <- c(levels(test$age),37)
train$age[test$age>=76] <- 37

c <- ggplot(data = test,aes(x=test$age,y=test$age,group=1))+geom_boxplot(col="coral")
e <- c+theme(panel.background = element_rect(fill = "Purple"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g


## now we finally got a age without any outlier

c <- ggplot(data = test,aes(x=test$capitalgain,y=test$capitalgain,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill="skyblue"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

h<- test$capitalgain[test$capitalgain<110]
boxplot(h)
which(test$age>=110)
median(test$capitalgain)
##change outlier with median
levels(test$capitalgain) <- c(levels(test$capitalgain),0)
test$capitalgain[test$capitalgain>=110] <- 0

c <- ggplot(data = test,aes(x=test$capitalgain,y=test$capitalgain,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill="skyblue"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

## now we finally got cpital gain without any outlier


c <- ggplot(data = test,aes(x=test$capitalloss,y=test$capitalloss,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill="lightyellow"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

h <- test$capitalloss[test$capitalloss<150]
boxplot(h)
median(test$capitalloss)
levels(test$capitalloss) <- c(levels(test$capitalloss),0)
test$capitalloss[test$capitalloss>=150] <- 0

c <- ggplot(data = test,aes(x=test$capitalloss,y=test$capitalloss,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill="lightyellow"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g
## now we finally got a cpitalloss without any outlier


c <- ggplot(data = test,aes(x=test$hoursperweek,y=test$hoursperweek,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill="lightyellow"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

h <- test$hoursperweek[test$hoursperweek<48]
boxplot(h)
k <- test$hoursperweek[test$hoursperweek>60]
boxplot(k)

median(test$hoursperweek)
levels(test$hoursperweek) <- c(levels(test$hoursperweek),40)
test$hoursperweek[test$hoursperweek>=48] <- 40
test$hoursperweek[test$hoursperweek<=60] <- 40

c <- ggplot(data = test,aes(x=test$hoursperweek,y=test$hoursperweek,group=1))+geom_boxplot()
e <- c+theme(panel.background = element_rect(fill="lightyellow"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

## no outlier



### so  ow there is no outlier in data set so we will proceed furthure 


##model building


library(kernlab)
model1 <- ksvm(train$Salary~.,data=train,kernel="rbfdot")
model1
##16 %error
predict1 <- predict(model1,test)
library(caret)
library(gmodels)
confusionMatrix(predict1,test$Salary)
#Accuracy : 0.8266 
#Sensitivity : 0.9211          
#Specificity : 0.5362 
#Kappa : 0.4943  
#consider this model

model2 <- ksvm(train$Salary~.,data=train,kernel="polydot")
model2
##17 %error
predict2 <- predict(model2,test)
confusionMatrix(predict2,test$Salary)
#Accuracy : 0.823
#Sensitivity : 0.9077          
#Specificity : 0.5632 
#Kappa : 0.4966
#consider this model

model3 <- ksvm(train$Salary~.,data=train,kernel="vanilladot")
model3
#Training error : 0.176121 
predict3 <- predict(model3,test)
confusionMatrix(predict3,test$Salary)
#Accuracy : 0.823 
#Sensitivity : 0.9077          
#Specificity : 0.5632 
#Kappa : 0.4966 
#consider this model
#model4 <- ksvm(train$Salary~.,data=train,kernel="tanhdot")
#model4
## size is 3.8 gb and my ram is only 4 gb so i cant use this kernel for this data 
#predict4 <- predict(model4,test)
#confusionMatrix(predict4,test$Salary)


model5 <- ksvm(train$Salary~.,data=train,kernel="laplacedot")
model5
#Training error : 0.164815
predict5 <- predict(model5,test)
confusionMatrix(predict5,test$Salary)
#Accuracy : 0.8274  
#Sensitivity : 0.9314          
#Specificity : 0.5081 
#Kappa : 0.4856


model6 <- ksvm(train$Salary~.,data=train,kernel="besseldot")
model6
#Training error : 0.188953 
predict6 <- predict(model6,test)
confusionMatrix(predict6,test$Salary)
#Sensitivity : 0.8787         
#Specificity : 0.5486
# Kappa : 0.439
## not consider this model bcz we already got hifh accuracy with good Sensitivity 
#Specificity &Kappa value

#model7 <- ksvm(train$Salary~.,data=train,kernel="anovadot")
#model7

#predict7 <- predict(model7,test)
#confusionMatrix(predict7,test$Salary)



#model8 <- ksvm(train$Salary~.,data=train,kernel="splinedot")
#model8

#predict8 <- predict(model8,test)
#confusionMatrix(predict8,test$Salary)
 
##i have not used  kernel="anovadot" & kernel="splinedot" bcz it take alot of time 
# in my laptop and when i tried my lapotop gone hang 
## this thing also depend on business object that when you want
# accuracy or when you want performance

##so we will take any model 5 3 2 1 for prediction
