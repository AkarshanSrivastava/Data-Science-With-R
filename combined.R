train <- read.csv(file.choose())
test <- read.csv(file.choose())
prop.table(table(train$Salary))
prop.table(table(test$Salary))
## % of salary
######EDA
library(psych)
describe(train)
### we got 1st 2nd 3rd 4th BM and also got max min all the thing by this function
describe(test)

library(Amelia)
missmap(train)
##no missing value 0%
missmap(test)
##no missing value 0%

### graph
library(ggplot2)
windows()
ggplot(data = train,aes(x=train$age,col=train$Salary))+geom_histogram(col="red")
##age between 35 to 45 getting highest salery  and above 50 getting lowest salary
### 
ggplot(data = test,aes(x=test$age,col=test$Salary))+geom_histogram(col="red")



ggplot(data=train, aes(train$age, colour = train$Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by salary")
#### age between 25 to 50 getting highest salary 
## age between 50 to 75 getting lowest salry 



ggplot(data=train, aes(train$capitalgain, colour = train$Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="capitalgain  by salary")

ggplot(data=train, aes(train$capitalloss, colour = train$Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="capitallosss Distribution by salary")

ggplot(data=train, aes(train$hoursperweek, colour = train$Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="hoursperweek  by Salary")



c <- ggplot(data=train, aes(train$age, fill=train$Salary, color=train$Salary)) +
  geom_histogram(binwidth = 1) + labs(title="relationship by salary")
c + theme_bw()


P <- ggplot(data=train, aes(x=train$hoursperweek, fill=train$Salary, color=train$Salary)) +
  geom_histogram(binwidth = 1) + labs(title="hour by salary")
P + theme_bw()


ggplot(data=train, aes(train$age, colour = Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="age by salary")

ggplot(data=train, aes(x=train$relationship,fill=Salary )) +
  geom_bar() + labs(title="hour by salary")


ggplot(data=train, aes(x=train$relationship,fill=Salary )) +
  geom_bar() + 
  facet_grid(.~Salary)+labs(title="hour by salary")

## salary divide by hour 

ggplot(data=train, aes(x=train$workclass,fill=Salary )) +
  geom_bar() + 
  facet_grid(.~Salary)+labs(title="workclass by salary")

ggplot(data = train,aes(x=train$age,fill=train$Salary))+geom_bar()
###salary according to age 

ggplot(data =train,aes(x=train$age,y=train$education,fill=train$Salary))+geom_boxplot(col="blue")

##education age wrt salary


ggplot(data = train,aes(x=age,y=education,col=Salary))+geom_jitter()

##education age wrt salary

##no uoutlier
library(e1071)
model1 <- naiveBayes(train$Salary~.,data = train)
summary(model1)
print(model1)

pre <- predict(model1,train)
pre1 <- predict(model1,test)
library(caret)
confusionMatrix(table(pre,train$Salary))
##82
confusionMatrix(table(pre1,test$Salary))
##82%

##good model no overfit and underfitb pro

model2 <- naiveBayes(train$Salary~.,data = train,laplace = 5)
summary(model2)
print(model2)

pre2 <- predict(model2,train)
pre3 <- predict(model2,test)
confusionMatrix(table(pre2,train$Salary))
##82%
confusionMatrix(table(pre3,test$Salary))
##81.8

##also a good model

library(caret)
attach(train)
train_control1 <- trainControl(method = "cv",number =20)
library(e1071)

model3 <-naiveBayes(Salary~.,data = train,trControl=train_control1,method="nb") 
print(model3)
summary(model3)


predict1 <- predict(model3,train[,-14])

confusionMatrix(predict1,train$Salary)
###0.8225  
prdict2 <- predict(model3,test[,-14])
confusionMatrix(prdict2,test$Salary)
###0.82  



## we will take model 3 or model 1 for the prediction 



