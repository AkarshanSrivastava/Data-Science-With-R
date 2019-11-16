library(Amelia)
library(psych)
library(neuralnet)
library(ie2misc)
library(caret)
#library(gmodels)

concrete <- read.csv("C://Users//Sanchi//Desktop//Assignment//Complete//R//neural network//concrete.csv")

missmap(concrete)
##0% missing value 
describe(concrete)
##value is to mch high so we will do normalization

normal <- as.data.frame(scale(concrete))
str(normal)
describe(normal)
## we got 1st 2nd 3rd 4th BM
summary(normal)


missmap(normal)
##0% missing value 
set.seed(2223)
sample_data<- sample(nrow(normal),(nrow(normal)*.7))
train <- normal[sample_data,]
test <- normal[-sample_data,]
library(neuralnet)
model1 <- neuralnet(train$strength~.,data = train,hidden = 5)
model1
summary(model1)
model1$result.matrix
plot(model1)
#ERROR 33.4

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(model1)
str(model1)
pred0 <- predict(model1,test)
pred00 <- compute(model1,test)
pred00$net.result
pred00$neurons
rmse(pred0,test$strength)
##.34
cor(pred00$net.result,test$strength)
##.94 strong


model2 <- neuralnet(train$strength ~.,data =train,hidden = c(4,3))

model2
summary(model2)
model2$result.matrix
plot(model2)

##error 38
pred11 <- predict(model2,test)
pred1 <- compute(model2,test)
rmse(pred11,test$strength)
##.38
cor(pred1$net.result,test$strength)
##.92

model3 <- neuralnet(train$strength~.,data = train,hidden = 7)
model3
summary(model3)
plot(model3)
##27.29
predict1 <- predict(model3,test)
predict11 <- compute(model3,test)
rmse(predict1,test$strength)
##.31
cor(predict11$net.result,test$strength)
##.95

##we will consider model 3