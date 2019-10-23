data()
data("iris")
View(iris)


##spliting the data
sample_data <- sample(1:nrow(iris),(nrow(iris)*.8))
train <- iris[sample_data,]
test <- iris[-sample_data,]
###model building

library(randomForest)
model1 <- randomForest(train$Species~.,data = train,na.action = na.roughfix,importance=T)
pre1 <- predict(model1,train)
mean(pre1==train$Species)
pre2 <- predict(model1,test)
mean(pre2==test$Species)

library(caret)
library(gmodels)
CrossTable(pre2,test$Species)
confusionMatrix(pre2,test$Species)
### it wrong wrt to virginica


train_control <- trainControl(method ="oob",number = 10 )

model10 <- randomForest(train$Species~.,data = train,trainControl=train_control,na.action = na.roughfix,importance=T)
pre10 <- predict(model10,train)
mean(pre10==train$Species)
pre20 <- predict(model10,test)
mean(pre20==test$Species)

library(caret)
library(gmodels)
CrossTable(pre20,test$Species)
confusionMatrix(pre20,test$Species)


### both are the best model we will take anyone 
