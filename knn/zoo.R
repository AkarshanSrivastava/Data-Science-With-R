###dependent -> type
zoo <- read.csv(file.choose())
zoo <- zoo[,-1]
View(zoo)
str(zoo)
summary(zoo)
#table(zoo$animal.name)
round(prop.table(table(zoo$type))*100,1)
##zoo$animal.name <- as.integer(zoo$animal.name)
normalize1 <- function(x){
  return ((x-min(x))/(range(x)))
}
normal2 <- as.data.frame(lapply(zoo[,1:16],normalize1))
normal1 <- as.data.frame(scale(zoo[,1:16]))
summary(normal2)
all(is.na(normal2))
### sample

set.seed(123)
sample_data<- sample(1:nrow(normal2),size = nrow(normal2)*.07,replace = FALSE)
#70%
#data <- createDataPartition(zoo$animal.name,p=.7,list = FALSE)
#train <- zoo[data,]
#test <- zoo[-data,]

### by using sample method so we found that it give 30 % in train and 70 % 
##in testing so we divide data manually
zoo_train <- zoo[1:70,]
##70
zoo_test <- zoo[71:101,]
#30
##zoo$animal.name <- factor(zoo$animal.name)
str(zoo)
##LABEL
ZOO_train_lebel <- zoo[1:70,17]
zoo_test_lebel <- zoo[71:101,17]

#ZOO_train_lebel <- zoo[data,1]
#zoo_test_lebel <- zoo[-data,1]


i=1                          # declaration to initiate for loop
j=1                     # declaration to initiate for loop

for (i in 1:28){ 
  predict1 <-  knn(train=zoo_train, test=zoo_test, cl=ZOO_train_lebel, k=i)
  j[i] <- 100 * sum(zoo_test_lebel == predict1)/NROW(zoo_test_lebel)
  k=i  
  cat(k,'=',j[i],'\n')       # to print % accuracy 
}



##model build

library(class)
detach()
sqrt(101)/2  = 5
model1 <- knn(train=zoo_train,test = zoo_test,cl=ZOO_train_lebel,k=5)
sum(is.na (zoo))
all(is.na(zoo))
## in this we worked on distance so it is imp to covert factor to int 
library(gmodels)
cross <- CrossTable(model1,zoo_test_lebel)
#table(model1,zoo_test_lebel)
library(caret)
#u<- union(model1,zoo_test_lebel)
#t <- table(factor(model1,u),factor(zoo_test_lebel,u))
#confusionMatrix(t)
#model2 <- as.data.frame(as.integer(model1))
confusionMatrix(table(model1,zoo_test_lebel))
#table(model2)
#100*(sum(zoo_test_lebel=model1,zoo_test_lebel))

cm = as.matrix(table(Actual = zoo_test_lebel, Predicted = model1))
sum(diag(cm))/length(zoo_test_lebel)
###77%

model2 <- knn(train=zoo_train,test = zoo_test,cl=ZOO_train_lebel,k=1)
confusionMatrix(table(model2,zoo_test_lebel))
cn=as.matrix(table(zoo_test_lebel,model2))
sum(diag(cn))/length(zoo_test_lebel)
##93%

model3 <- knn(train=zoo_train,test = zoo_test,cl=ZOO_train_lebel,k=2)
confusionMatrix(table(model3,zoo_test_lebel))
cn=as.matrix(table(zoo_test_lebel,model3))
sum(diag(cn))/length(zoo_test_lebel)
##77%

##we find k=1 best so model2 is best

final<- as.data.frame(model2)
plot(model2,col="green")
windows()
library(ggplot2)
ggplot(final,aes(x=1))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

