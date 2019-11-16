forest <- read.csv("C://Users//Acer//Desktop//Assignment//pending//support vector machine//forestfires.csv")
View(forest)
##build model for area
### we found that month and day already converted so we will remove it
### also remove size
forest <- forest[,-c(1,2,31)]
View(forest)
###in this i am going to prdict size 
##EDA
str(forest)
table(forest$area)
mean(forest$area)
c <- ifelse(forest$area>12.84729,yes = "1",no="0")
c <- as.data.frame(as.factor(c))
forest <- cbind(c,forest)
colnames(forest)[1] <- "area"
forest <- forest[,c(-10)]
str(forest)

library(psych)
describe(forest)
##1st 2nd 3rd 4th BM 
library(Amelia)
missmap(forest)
#0% missing value



####in this data set i have already build a model with tarin(70%) and check with test
# but not got a good accuracy and kappa value so now i will build my model with 100%
#data and check with randomlly selected 30% data 


#spliting the data
set.seed(124)
sample_dta <- sample(nrow(forest),(nrow(forest)*.7))
train <- forest[sample_dta,]
test <- forest[-sample_dta,]
library(caret)
tr_contrl <- trainControl(method = "cv",number = 10)

## model building
library(kernlab)
model1 <- ksvm(forest$area~.,data=forest,kernel="rbfdot",trainControl=tr_contrl)
model1
##eror 14%
p1<- predict(model1,test)
confusionMatrix(p1,test$area)
#Sensitivity : 1.0000          
#Specificity : 0.0000  
#Kappa : 0  
#Accuracy : 0.8654 
#never take this model
  library(gmodels)
CrossTable(p1,test$area)


model2 <- ksvm(forest$area~.,data=forest,kernel="polydot",trainControl=tr_contrl)
model2
##eror 14%
p2<- predict(model2,test)
confusionMatrix(p2,test$area)
#Sensitivity : 1.0000          
#Specificity : 0.0000  
#Kappa : 0  
#Accuracy : 0.8654 
#never take this model
CrossTable(p2,test$area)


model3 <- ksvm(forest$area~.,data=forest,kernel="vanilladot",trainControl=tr_contrl)
model3
##eror 14%
p3<- predict(model3,test)
confusionMatrix(p3,test$area)
#Sensitivity : 1.0000          
#Specificity : 0.0000  
#Kappa : 0  
#Accuracy : 0.8654 
#never take this model
CrossTable(p3,test$area)



model4 <- ksvm(forest$area~.,data=forest,kernel="tanhdot",trainControl=tr_contrl)
model4
##eror 25%
p4<- predict(model4,test)
mean(p4==test$area)
confusionMatrix(p4,test$area)
#Sensitivity : .8667
#Specificity : 0.1429
#Kappa :.0095
#Accuracy : 0.7692
#consider
CrossTable(p2,test$area)


model5 <- ksvm(forest$area~.,data=forest,kernel="laplacedot",trainControl=tr_contrl)
model5
##eror 14%
p5<- predict(model5,test)
mean(p5==test$area)
##86
confusionMatrix(p5,test$area)
#Sensitivity : .8667
#Specificity : 0.1429
#Kappa :.0
#Accuracy : 0.7692
#not take this model
CrossTable(p5,test$area)


model6 <- ksvm(forest$area~.,data=forest,kernel="besseldot",trainControl=tr_contrl)
model6
##eror 14%
p6<- predict(model6,test)
mean(p6==test$area)
##86
confusionMatrix(p6,test$area)
#Sensitivity : .8667
#Specificity : 0.1429
#Kappa :.0
#Accuracy : 0.7692
#not take this model
CrossTable(p5,test$area)


model7 <- ksvm(forest$area~.,data=forest,kernel="anovadot",trainControl=tr_contrl)
model7
##eror 14%
p7<- predict(model7,test)
mean(p7==test$area)
##86
confusionMatrix(p7,test$area)
#Sensitivity : 1
#Specificity : 0
#Kappa :.0
#not take this model
CrossTable(p7,test$area)



model8 <- ksvm(forest$area~.,data=forest,kernel="splinedot",trainControl=tr_contrl)
model8
##eror 10%
p8<- predict(model8,test)
mean(p8==test$area)
##91
confusionMatrix(p8,test$area)
#Sensitivity : .96
#Specificity : .57
#Kappa :.58
#take this model
CrossTable(p8,test$area)
### we will consider model 8 for prediction 