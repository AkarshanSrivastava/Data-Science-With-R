fraud <- read.csv("C://Users//Acer//Desktop//Assignment//pending//random forest//Fraud_check.csv")
View(fraud)
c <- ifelse(fraud$Taxable.Income>30000,yes = "Good",no="Risky")
fraud <- cbind(c,fraud)
colnames(fraud)[1] <- "Taxable.Income"
fraud <- fraud[,c(-3,-4)]


###spliting the data 

set.seed(222)
sample_data <- sample(1:nrow(fraud),(nrow(fraud)*.7))
train <- fraud[sample_data,]
test <- fraud[-sample_data,]


###model building
library(randomForest)
model1 <- randomForest(train$Taxable.Income~.,train,na.action=na.roughfix,importance = T)
summary(model1)
model1
varImpPlot(model1)
library(caret)
varImp(model1)
model1$importance
predict1 <- predict(model1,train)
predict2 <- predict(model1,test)
mean(predict1==train$Taxable.Income)
mean(predict2==test$Taxable.Income)
### overfit pro
### boosting 
model2 <- randomForest(train$Taxable.Income~.,train,trials=10,na.action = na.roughfix,importance=TRUE)
predict3 <- predict(model2,train)
predict4 <- predict(model2,test)
mean(predict3==train$Taxable.Income)
mean(predict4==test$Taxable.Income)
###overfit is there

model3 <- randomForest(train$Taxable.Income~.,train,trials=11,na.action = na.roughfix,importance=TRUE)
predict5 <- predict(model3,train)
predict6 <- predict(model3,test)
mean(predict5==train$Taxable.Income)
mean(predict6==test$Taxable.Income)
## overfit is there 

## now we will take some more test data 

set.seed(123)
sample_data2 <- sample(nrow(fraud),(.6*nrow(fraud)))
train1 <- fraud[sample_data2,]
test1 <- fraud[-sample_data2,]
## model buiding
model4 <- randomForest(train1$Taxable.Income~.,train1,na.action = na.roughfix,importance=TRUE)
predict7 <- predict(model4,train1)
predict8 <- predict(model4,test1)
mean(predict7==train1$Taxable.Income)
mean(predict8==test1$Taxable.Income)
##overfit is there

model5 <- randomForest(train1$Taxable.Income~.,train1,trials=10,na.action = na.roughfix,importance=TRUE)
predict9 <- predict(model5,train1)
predict10 <- predict(model5,test1)
mean(predict9==train1$Taxable.Income)
mean(predict10==test1$Taxable.Income)
 
####overfit 



## take some more data in trainig

set.seed(123)
sample_data3 <- sample(nrow(fraud),(.8*nrow(fraud)))
train2 <- fraud[sample_data3,]
test2 <- fraud[-sample_data3,]
## model buiding
model6 <- randomForest(train2$Taxable.Income~.,train2,na.action = na.roughfix,importance=TRUE)
predict11 <- predict(model6,train2)
predict12 <- predict(model6,test2)
mean(predict11==train2$Taxable.Income)
mean(predict11==test2$Taxable.Income)

train_control <- trainControl(method = "oob",number = 10,verboseIter = T)
bestmtry <- tuneRF(fraud,fraud$Taxable.Income,stepFactor = 1.2,prove = .002,trace = T,plot = T)

### 
set.seed(13)
sample_data4 <- sample(nrow(fraud),(.7*nrow(fraud)))
train3 <- fraud[sample_data4,]
test3 <- fraud[-sample_data4,]
## model buiding
model7 <- randomForest(train3$Taxable.Income~.,train3,trials =10,mtry=bestmtry,trainControl=train_control,na.action = na.roughfix,importance=TRUE)
predict13 <- predict(model7,train3)
predict14 <- predict(model7,test3)
mean(predict13==train3$Taxable.Income)
mean(predict14==test3$Taxable.Income)
### so now we get a good model
model7$importance
model7$oob.times  
varImp(model7)
varImpPlot(model7)
library(gmodels)
CrossTable(predict13,train3$Taxable.Income)
CrossTable(predict14,test3$Taxable.Income)
confusionMatrix(predict14,test3$Taxable.Income)
### itb is a  good model bcz it do not predict wrt to Risky