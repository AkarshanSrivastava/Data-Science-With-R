##dependent variable is salary
##data salary train
salary <- read.csv(file.choose(),stringsAsFactors = F)
View(salary)
table(salary$Salary)
str(salary)
salary$Salary <- as.factor(salary$Salary)
str(salary)
####two class pro
library(psych)
describe(salary)
####checking missing value 
library(Amelia)
missmap(salary)
##0% missing value 

library(ggplot2)
windows()
ggplot(data = salary,aes(x=salary$age,col=salary$Salary))+geom_histogram(col="red")
##age between 35 to 45 getting highest salery 

ggplot(data=salary, aes(salary$age, colour = salary$Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by salary")

ggplot(data=salary, aes(salary$educationno, colour = salary$Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="Educationno  by salary")
table(salary$educationno)

ggplot(data=salary, aes(salary$capitalgain, colour = salary$Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="capitalgain  by salary")

ggplot(data=salary, aes(salary$capitalloss, colour = salary$Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="capitallosss Distribution by salary")

ggplot(data=salary, aes(salary$hoursperweek, colour = salary$Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="hoursperweek  by Salary")



c <- ggplot(data=salary, aes(salary$age, fill=salary$Salary, color=salary$Salary)) +
  geom_histogram(binwidth = 1) + labs(title="relationship by salary")
c + theme_bw()


P <- ggplot(data=salary, aes(x=salary$hoursperweek, fill=salary$Salary, color=salary$Salary)) +
  geom_histogram(binwidth = 1) + labs(title="hour by salary")
P + theme_bw()


ggplot(data=salary, aes(salary$age, colour = Salary)) +
  geom_freqpoly(binwidth = 1) + labs(title="age by salary")

ggplot(data=salary, aes(x=salary$relationship,fill=Salary )) +
  geom_bar() + labs(title="hour by salary")


ggplot(data=salary, aes(x=salary$relationship,fill=Salary )) +
  geom_bar() + 
  facet_grid(.~Salary)+labs(title="hour by salary")


ggplot(data=salary, aes(x=salary$workclass,fill=Salary )) +
  geom_bar() + 
  facet_grid(.~Salary)+labs(title="workclass by salary")


####spliting the data
set.seed(456)
sampledata <- sample(1:nrow(salary),size =nrow(salary)*.8,replace = T)

train <- salary[sampledata,]
test <- salary[-sampledata,]


prop.table(table(salary$Salary))*100

prop.table(table(train$Salary))*100

prop.table(table(test$Salary))*100

library(e1071)
model_nb <-naiveBayes(train$Salary~.,data = train) 
print(model_nb)
summary(model_nb)
predict2 <- predict(model_nb,test)
mean(predict2==test$Salary)
library(caret)
confusionMatrix(predict2,test$Salary)
####83% accuracy
predict3 <- predict(model_nb,train)
confusionMatrix(predict3,train$Salary)
##81 %

model_nb1 <-naiveBayes(train$Salary~.,data = train,laplace = 10) 
print(model_nb1)
summary(model_nb1)
predict3 <- predict(model_nb1,test)
mean(predict3==test$Salary)


library(caret)
attach(salary)
train_control1 <- trainControl(method = "cv",number =20)
library(e1071)

model1 <-naiveBayes(salary$Salary~.,data = salary,trControl=train_control1,method="nb") 
print(model1)
summary(model1)


predict1 <- predict(model1,salary[,-14])

confusionMatrix(predict1,salary$Salary)

mean(predict1==salary$Salary)

install.packages("hydroGOF")
library(hydroGOF)
sim <-as.integer(result$Predicted)
obs <-as.integer(result$Actual)
mae(sim,obs)


colnames(salary)
######with some clm
salary2 <- salary[,c(1,2,3,6,8,12,13,14)]
str(salary2)

sampledata <- sample(1:nrow(salary2),size=nrow(salary2)*.7,replace = T)
prop.table(table(salary2$Salary))
train1 <-salary2[sampledata,]
test1 <- salary2[-sampledata,]

prop.table(table(train1$Salary))
prop.table(table(test1$Salary))
model5 <- naiveBayes(Salary~age+workclass+education+occupation+
                       race+hoursperweek+native,data = train1)
str(model5)
print(model5)
summary(model5)
predict5 <- predict(model5,train1)
predict6 <- predict(model5,test1)

confusionMatrix(table(predict5,train1$Salary))
confusionMatrix(table(predict6,test1$Salary))



### we will take model_nb as our predict or final model


