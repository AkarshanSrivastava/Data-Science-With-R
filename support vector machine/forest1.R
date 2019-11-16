forest <- read.csv("C://Users//Acer//Desktop//Assignment//pending//support vector machine//forestfires.csv")
View(forest)
### we found that month and day already converted so we will remove it
forest <- forest[,-c(1,2)]
View(forest)
###in this i am going to prdict size 
##EDA
str(forest)
library(psych)
describe(forest)
##1st 2nd 3rd 4th BM 
## go for normalization 
normal <- as.data.frame(scale(forest[,c(1:28)]))
describe(normal)


library(Amelia)
missmap(normal)
missmap(forest)
##0 % missing data
## we want to add categorical data in normal 
forest1 <- forest[,c(29)]
## bind with normal data 
normal <- cbind(normal,forest1)



## graphical technique 
## i am going to use ggplot
library(ggplot2)
c <- ggplot(data = forest,aes(x=forest$FFMC,fill=forest$size_category))+geom_freqpoly(col="coral")+labs(tittle="FFMC wrt to Forest size")
d <- c+facet_grid(.~forest$size_category)
e <- d+theme(panel.background = element_rect(fill = "black"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g
## from this we can say that FFMC contain max small area 
## value is also less in large

c <- ggplot(data = forest,aes(x=forest$DMC,fill=forest$size_category))+geom_histogram(col="coral")+labs(tittle="DMc wrt to Forest size")
d <- c+facet_grid(.~forest$size_category)
e <- d+theme(panel.background = element_rect(fill = "magenta"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Brown"))
g

####DMC value is high in small area
##DMC max in small area
c <- ggplot(data = forest,aes(x=forest$DC,fill=forest$size_category))+geom_histogram()
d<- c
e <- d+theme(panel.background = element_rect())
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Black"))
g

##DC is large from 600 to 700
##but itcontain max small

c <- ggplot(data = forest,aes(x=forest$ISI,fill=forest$size_category))+geom_histogram()
d <- c+facet_grid(.~forest$size_category)
e <- d+theme(panel.background = element_rect(fill="tan"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Black"))
g


c <- ggplot(data = forest,aes(x=forest$ISI,fill=forest$size_category))+geom_histogram(binwidth = 20)
d <- c+facet_grid(.~forest$size_category)
e <- d+theme(panel.background = element_rect(fill="tan"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "Black"))
g

sample_data <- sample(nrow(normal),(nrow(normal)*.7))
## now we will split the data with categorical variable 
train <- normal[sample_data,]
test <-  normal[-sample_data,]


### model buuilding
library(kernlab)
model1 <- ksvm(train$forest1~.,data=train,kernel="rbfdot")
summary(model1)
model1
##22.7% error
predict1 <- predict(model1,test)
mean(predict1==test$forest1)
##77% accuracy
library(caret)
confusionMatrix(predict1,test$forest1)
library(gmodels)
CrossTable(predict1,test$forest1)
##not good bcz  Sensitivity : 0.17073 Specificity : 0.99130 are to much diffrent 
##kappa is also to much low


model2 <- ksvm(train$forest1~.,data=train,kernel="polydot")
model2
##12 % error
predict2 <- predict(model2,test)
confusionMatrix(predict2,test$forest1)
## i will take this model bcz
#Sensitivity : 0.6585          
#Specificity : 0.9913
#Kappa : 0.7237          
##everthing is good but i will check with other kernel 

model3 <- ksvm(train$forest1~.,data=train,kernel="vanilladot")
model3
##12 % error
predict3 <- predict(model3,test)
confusionMatrix(predict3,test$forest1)
#Sensitivity : 0.6585          
#Specificity : 0.9913   
#Kappa : 0.7237   

##THIS IS ALSO A GOOD MODEL


model4 <- ksvm(train$forest1~.,data=train,kernel="tanhdot")
model4
##41.5 % error
predict4 <- predict(model4,test)
confusionMatrix(predict4,test$forest1)
##64% Accuracy
#Sensitivity : 0.2439         
#Specificity : 0.7913  
#Kappa : 0.0373 

## we cant not take this model

model5 <- ksvm(train$forest1~.,data=train,kernel="laplacedot")
model5
##25% error
predict5 <- predict(model5,test)
confusionMatrix(predict5,test$forest1)
## kappa is not good so we cant take this model
## also accuracy is low we have already get high accuracy so we cant take this model


model6 <- ksvm(train$forest1~.,data=train,kernel="besseldot")
model6
##36.5% error
predict6 <- predict(model6,test)
confusionMatrix(predict5,test$forest1)
## kappa is not good so we cant take this model
## also accuracy is low we have already get high accuracy so we cant take this model


model7 <- ksvm(train$forest1~.,data=train,kernel="anovadot")
model7
##5% error
predict7 <- predict(model7,test)
confusionMatrix(predict7,test$forest1)
##Kappa : 0.8795
#Sensitivity : 0.8537          
##Specificity : 0.9913 
#Accuracy : 0.9551 

## it is a very good model
##check for furthure model

model8 <- ksvm(train$forest1~.,data=train,kernel="splinedot")
model8
##7% error
predict8 <- predict(model8,test)
confusionMatrix(predict8,test$forest1)
##Kappa : 0.1972  
#Sensitivity : 0.5366         
#Specificity : 0.6870 
#Accuracy : 0.6474
## kappa is not good so we cant take this model
## also accuracy is low we have already get high accuracy so we cant take this model

model9 <- ksvm(train$forest1~.,data=train,kernel="besseldot")
model9
##36% error
predict9 <- predict(model9,test)
confusionMatrix(predict9,test$forest1)
#Sensitivity : 0.26829         
#Specificity : 0.88696 
#Kappa : 0.1791          
#Accuracy : 0.7244   
## kappa is not good so we cant take this model
## also accuracy is low we have already get high accuracy so we cant take this model

## we will consider model 7 for  prediction 
CrossTable(predict7,test$forest1)



#### for improving more accuracy we will take 80% data in train & 20% in teat 
### i will also use traincontro
set.seed(333)
sample_data1 <- sample(nrow(normal),(nrow(normal)*.8))
train1 <- normal[sample_data1,]
test1 <- normal[-sample_data1,]
tr_cntrl <- trainControl(method = "cv",number = 10)
###model building
model10 <- ksvm(train1$forest1~.,data=train1,kernel="rbfdot",trainControl=tr_cntrl)
model10
###20% error
## we are not going to take this model bcz we ghave already got high accuracy
#that is 95% in model7
model11 <- ksvm(train1$forest1~.,data=train1,kernel="polydot",trainControl=tr_cntrl)
model11
##8% error
## we are not going to take this model bcz we ghave already got high accuracy
#that is 95% in model7
model12 <- ksvm(train1$forest1~.,data=train1,kernel="vanilladot",trainControl=tr_cntrl)
model12
##8% error
## we are not going to take this model bcz we ghave already got high accuracy
#that is 95% in model7

model13 <- ksvm(train1$forest1~.,data=train1,kernel="tanhdot",trainControl=tr_cntrl)
model13
##38% error
predict13 <-predict(model13,test1) 
confusionMatrix(predict13,test1$forest1)
##only 68%
## we are not going to take this model bcz we ghave already got high accuracy
#that is 95% in model7

model14 <- ksvm(train1$forest1~.,data=train1,kernel="laplacedot",trainControl=tr_cntrl)
model14
##23% error
## we are not going to take this model bcz we ghave already got high accuracy
#that is 95% in model7

model15 <- ksvm(train1$forest1~.,data=train1,kernel="besseldot",trainControl=tr_cntrl)
model15
##35% error
## we are not going to take this model bcz we ghave already got high accuracy
#that is 95% in model7

model16 <- ksvm(train1$forest1~.,data=train1,kernel="anovadot",trainControl=tr_cntrl)
model16
##4% error
predict16 <-predict(model16,test1) 
confusionMatrix(predict16,test1$forest1)
## 93.31
## same as model7 we will take anyone

model17 <- ksvm(train1$forest1~.,data=train1,kernel="splinedot",trainControl=tr_cntrl)
model17
##11% error




##### final ###
##we will take model 7 or model 16 for prediction  bcz there is no diffrence between them 


