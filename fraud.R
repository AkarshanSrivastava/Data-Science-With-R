fraud <- read.csv("C://Users//Acer//Desktop//Assignment//pending//DECISION TREE//Fraud_check.csv")
View(fraud)
####EDA##
c <- data.frame(ifelse(fraud$Taxable.Income>=30000,yes = "Good",no="Risky"))
fraud <- cbind(fraud,c)
colnames(fraud)[7] <- "Taxable.Income"
fraud <- fraud[,c(-2,-3)]

library(psych)
describe(fraud)

###we got mean median range skew kurtosis sd ###
library(Amelia)
missmap(fraud)
###0% missing value 

##GRAPH
library(ggplot2)
c <- ggplot(data = fraud,aes(x=fraud$Work.Experience,fill=fraud$Taxable.Income))+
  geom_histogram()
c+theme_bw()
### we can say that if exp is more so there is no chance of Risk
ggplot(data=fraud, aes(x=fraud$Undergrad,fill=fraud$Taxable.Income)) +
  geom_bar() + 
  facet_grid(.~fraud$Undergrad)+labs(title="Taxable income wrt to education ")
### so we can say that this is equal
ggplot(data = fraud,aes(x=fraud$City.Population,fill=fraud$Taxable.Income))+
  geom_histogram()


######spliting the data


set.seed(123)
sampledata <- sample(nrow(fraud),(nrow(fraud)*.7),replace = FALSE)
train1 <- fraud[sampledata,]
test1 <- fraud[-sampledata,]

####model building
library(C50)
model1 <- C5.0(train1[,c(-5)],train1$Taxable.Income)
summary(model1)
model1
print(model1)
plot(model1)

##predict
predict1 <- predict(model1,train1)
mean(predict1==train1$Taxable.Income)
library(caret)
confusionMatrix(predict1,train1$Taxable.Income)
library(gmodels)
CrossTable(predict1,train1$Taxable.Income)
predict2 <- predict(model1,test1)
mean(predict2==test1$Taxable.Income)
confusionMatrix(predict2,test1$Taxable.Income)
CrossTable(predict2,test1$Taxable.Income)
#####it is a very good model bcz it cant predict wrong in terms of risk

#####using tree package 

library(tree)
model2 <- tree(train1$Taxable.Income~.,data = train1)
print(model2)
summary(model2)$used
plot(model2)##single node tree

predict3 <- predict(model2,train1,type = "class")
predict4 <- predict(model2,test1,type = "class")
table(predict=predict3,actual=train1$Taxable.Income)
table(predict=predict4,actual=test1$Taxable.Income)

confusionMatrix(predict3,train1$Taxable.Income)
###79
confusionMatrix(predict4,test1$Taxable.Income)
###80
###very good model



###k fold cross validation
library(caret)
train_control <- trainControl(method = "cv",number = 10)
model3 <- C5.0(train1[,c(-5)],train1$Taxable.Income, trControl=train_control,method="dt")

predict5 <- predict(model3,train1)
mean(predict5==train1$Taxable.Income)
##79
predict6 <- predict(model3,test1)
mean(predict6==test1$Taxable.Income)
##80
##very good model

train_control <- trainControl(method = "cv",number = 20)
model4 <- C5.0(train1[,c(-5)],train1$Taxable.Income, trControl=train_control,method="dt")

predict7 <- predict(model4,train1)
mean(predict7==train1$Taxable.Income)
##79
predict8 <- predict(model4,test1)
mean(predict8==test1$Taxable.Income)
##80

####this is also a very good model 

## we can take any model for future prediction