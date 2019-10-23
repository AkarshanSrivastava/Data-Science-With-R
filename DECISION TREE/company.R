company <- read.csv("C://Users//Acer//Desktop//Assignment//complete//DECISION TREE//Company_Data.csv")

View(company)
###Dependent variable is sales
##for the model building  we dont need urban and us
company <- company[,-c(10,11)]
str(company)
table(company$Sales)
#################    EDA    ################
company$Sales
##sales1 <- cut(company$Sales,breaks = c(2,4,8,12,16),labels=c("A","B","C","D"))
##company$Sales[1:5]
###sales1[1:5]
##str(sales1)
c<- as.data.frame((ifelse(test = company$Sales>=7.43,yes = "High",no="Low")))
company <- cbind(c,company)
colnames(company)[1] <- "sales1"
company <- company[,c(-2)]
str(company)

library(psych)
describe(company[,c(-1,-7)])
### we get 1st 2nd 3rd 4th BM
##checking missing Value 
library(Amelia)
missmap(company)
###0% missing value 
## graph
library(ggplot2)
windows()
ggplot(data = company,aes(x=company$CompPrice,fill=company$sales1))+geom_histogram(col="red")
###when computer price is 75 or 175 then there is higher chance of sales 
### if it is between 110 to 140 so there is higher chances of no sales 

ggplot(data=company, aes(company$Income, col= company$sales1)) +
  geom_freqpoly(binwidth = 1) + labs(title="sales wrt to income")
#### if income between 25 to 60 so its mean there is min sales (mean no sales)
## but when income increase then sales increase


p <- ggplot(data=company, aes(company$Advertising, y= company$sales1,shape="popular")) +
  geom_count(col="blue") + labs(title="sales by advertising ")
p+theme_bw()
##num of count wrt to advertising

c <- ggplot(data=company, aes(company$Population, fill=company$sales1)) +
  geom_histogram() + labs(title="sales wrt to population")
c + theme_bw()



ggplot(data=company, aes(x=company$Education,fill=company$sales1)) +
  geom_bar() + labs(title="sales wrt to education")


ggplot(data=company, aes(x=company$ShelveLoc,fill=company$sales1)) +
  geom_bar() + 
  facet_grid(.~sales1)+labs(title="sales wrt bad good mediu ")

ggplot(data=company, aes(x=company$Age,fill=company$sales1)) +
  geom_bar() + 
  facet_grid(.~sales1)+labs(title="sales wrt to age")



ggplot(data=company, aes(x=company$Income,fill=sales1 )) +
  geom_bar() + 
  facet_grid(.~sales1)+labs(title="workclass by salary")

##spliting the data
set.seed(123)
sample_data <- sample(1:nrow(company),(.07*nrow(company)),replace = FALSE)
test <- company[sample_data,]
train <- company[-sample_data,]

####set.seed function return the same sample
#####model building

library(C50)
model1 <- C5.0(train[,c(-1)],train$sales1)
summary(model1)
print(model1)
windows()
plot(model1)
install.packages("rattle")
library(rattle)
install.packages("RGtk2")
library(RGtk2)
rattle(company)
##another method for data mining


##install.packages("RColorBrewer")
##library(RColorBrewer)
##fancyRpartPlot(model1)
#printcp(model1)

##BOOSTING
model10 <-C5.0( train[,c(-1)],train$sales1, trials=10 )
summary(model10)


#####prediction

pre <- predict(model1,train)
mean(pre==train$sales1)
library(caret)
confusionMatrix(pre,train$sales1)
##94% accuracy
library(gmodels)
CrossTable(pre,train$sales1)

pre1 <- predict(model1,test)
mean(pre1==test$sales1)
library(caret)
confusionMatrix(pre1,test$sales1)
##71% 

library(caret)
train_control <- trainControl(method = "cv",number = 10)
model2 <- C5.0(train[,c(-1)],train$sales1, trControl=train_control,method="dt")

predict1 <- predict(model2,train)
mean(predict1==train$sales1)
##94%
predict2 <- predict(model2,test)
mean(predict2==test$sales1)
##71

#######with cart or rpart function##########
attach(train)
install.packages("rpart")
library(rpart)

model3 <- rpart(sales1~train$CompPrice+train$Income+train$Advertising+train$Population
                +train$Price+train$ShelveLoc+train$Age+train$Education,data = train,method = "class") 
model3
plot(model3)

text(model3)
summary(model3)
print(model3)
 

predict4 <- predict(model3,train)
library(gmodels)
rmse_mpg <- sqrt(mean((predict4-train$sales1)^2))
#####from here we can say that rpart is not good for factor or categorical data 

########tree package ###
library(tree)

###same accuracy mean our model is good model

model5 <- tree(company$sales1~.,data = company)
model5
dim(company)
summary(model5)

set.seed(123)
sample_data1 <- sample(1:nrow(company),(.07*nrow(company)),replace = TRUE)
test1 <- company[sample_data,]
train1 <- company[-sample_data,]
model6 <- tree(train1$sales1~.,data=train1)
model6
summary(model6)$used
names(company)[which(!(names(company) %in% summary(model6)$used))]
pred_tree <- predict(model6,train1,type = "class")
pred_test <- predict(model6,test1,type = "class")
table(predicted = pred_tree, actual = train1$sales1)
table(predicted = pred_test, actual = test1$sales1)

accuracy = function(actual, predicted) {
  mean(actual == predicted)
}
accuracy(predicted = pred_tree, actual = train1$sales1)
###.91
accuracy(predicted = pred_test, actual = test1$sales1)
####71

confusionMatrix(pred_tree,train1$sales1)
##91
confusionMatrix(pred_test,test1$sales1)
###71
library(gmodels)
CrossTable(pred_tree,train1$sales1)
CrossTable(pred_test,test$sales1)

####so there is overfitting problem

###so we will do boosting

##BOOSTING
model10 <-C5.0( train[,c(-1)],train$sales1, trials=10 )
summary(model10)


#####prediction

pre10 <- predict(model10,train)
mean(pre10==train$sales1)
library(caret)
confusionMatrix(pre10,train$sales1)
##100% accuracy
library(gmodels)
CrossTable(pre10,train$sales1)

pre11 <- predict(model10,test)
mean(pre11==test$sales1)
library(caret)
confusionMatrix(pre11,test$sales1)
##96.4
CrossTable(pre11,test$sales1)


## so it is a good model 


##dumyy data

dummy <- readxl::read_xlsx("C://Users//Acer//Desktop//Assignment//pending//DECISION TREE//dummy_data.xlsx")
str(train)
str(dummy)
dummy$sales1 <- as.factor(dummy$sales1)
attach(dummy)
dummy$CompPrice <- as.integer(dummy$CompPrice)
dummy$Income <- as.integer(dummy$Income)
dummy$Advertising <- as.integer(dummy$Advertising)
dummy$Population <- as.integer(dummy$Population)
dummy$Price <- as.integer(dummy$Price)
dummy$ShelveLoc <- as.factor(dummy$ShelveLoc)
dummy$Age <- as.integer(dummy$Age)
dummy$Education <- as.integer(dummy$Education)
class(dummy)
dummy <- as.data.frame(dummy)
pre12 <- predict(model1,dummy)
mean(pre12==dummy$sales1)
###dummy variable a data set which is created by me 

##BOOSTING
model15 <-C5.0( dummy[,c(-1)],dummy$sales1, trials=10 )
summary(model15)


#####prediction

pre15 <- predict(model15,dummy)
mean(pre15==dummy$sales1)
###85% accuracy 

## so it is a good model


##Now we will go fgor deployment 