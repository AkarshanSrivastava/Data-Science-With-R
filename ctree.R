install.packages("party")
library(party)
##require(party)
data("iris")
View(iris)
####target variable is species 

###EDA
library(psych)
describe(iris)
## find 1 2 3 4th BM
library(Amelia)
missmap(iris)
##0% missing value 

##graph

library(ggplot2)
c <- ggplot(data = iris,aes(x=iris$Sepal.Length,fill=iris$Species))+
  geom_histogram()+labs(title = "Species wrt to sepallength")
c+theme_bw()

### we can say that Versicolor has highest sepal length virginica has min
##setosa is avg.
c <- ggplot(data = iris,aes(x=iris$Sepal.Width,fill=iris$Species))+
  geom_histogram()+labs(title = "Species wrt to sepalwidth")
c+theme_bw()

c <- ggplot(data = iris,aes(x=iris$Petal.Length,fill=iris$Species))+
  geom_histogram()+labs(title = "Species wrt to petallength")
c+theme_bw()

c <- ggplot(data = iris,aes(x=iris$Petal.Width,fill=iris$Species))+
  geom_histogram()+labs(title = "Species wrt to petalwidth")
d <- c+theme(panel.background = element_rect(fill = "black"))
e <- d+theme(plot.title = element_text(hjust = .05,face = "bold"))
e

c <- ggplot(data = iris,aes(x=iris$Sepal.Length,fill=iris$Species))+
  geom_bar()+
  facet_grid(.~iris$Species)+labs(title="Species wrt to sepallength")
d <- c+theme(panel.background = element_rect(fill = "palegreen"))
e <- d+theme(plot.title = element_text(hjust = .05,face = "bold"))
e


c <- ggplot(data = iris,aes(x=iris$Petal.Length,fill=iris$Species))+
  geom_bar()+
  facet_grid(.~iris$Species)+labs(title="Species wrt to petallength")
d <- c+theme(panel.background = element_rect(fill = "palegreen"))
e <- d+theme(plot.title = element_text(hjust = .05,face = "bold"))
e


c <- ggplot(data = iris,aes(x=iris$Sepal.Length,y=iris$Sepal.Length,fill=iris$Species))+
  geom_boxplot()
d <- c+theme(panel.background = element_rect(fill = "palegreen"))
e <- d+theme(plot.title = element_text(hjust = .05,face = "bold",colour = "cadetblu"))
e

### no outlier in virginica

which(iris$Sepal.Length>7.8)



c <- ggplot(data = iris,aes(x=iris$Sepal.Width,y=iris$Sepal.Width,fill=iris$Species))+
  geom_boxplot()
d <- c+theme(panel.background = element_rect(fill = "yellow"))
e <- d+theme(plot.title = element_text(hjust = .05,face = "bold",colour = "cadetblu"))
e
grep(4.4,iris$Sepal.Width)
library(dplyr)
filter(iris,iris$Sepal.Width>4)
which(iris$Sepal.Width>4.3)

###detect 4 outlier

##outlier do not affected our data so proceed furthure

###spliting the data 

str(iris)
set.seed(123)
sample_data <- sample(1:nrow(iris),(nrow(iris)*.7))
train <- iris[sample_data,]
test <- iris[-sample_data,]
###model building
##model1 <- ctree(train$Species~.,data = train,controls = ctree_control(maxsurrogate = 0))

model1 <- ctree(train$Species~.,data = train)
model1
summary(model1)
plot(model1)

nodes(model1,1)

###
predict1 <- predict(model1,train)
predict2 <- predict(model1,test)
mean(predict1==train$Species)
###98
mean(predict2==test$Species)
###91

library(caret)
confusionMatrix(predict1,train$Species)
##98
confusionMatrix(predict2,test$Species)
##91

library(gmodels)
CrossTable(predict1,train$Species)
##100% accurate foe setosa & virginica
CrossTable(predict2,test$Species)
##100% accurate for setosa 
### so this is a good model


set.seed(123)
sample_data1 <- sample(1:nrow(iris),(nrow(iris)*.8))
train1 <- iris[sample_data1,]
test1 <- iris[-sample_data1,]
###model building
##model1 <- ctree(train$Species~.,data = train,controls = ctree_control(maxsurrogate = 0))

model2 <- ctree(train1$Species~.,data = train1)
model2
summary(model2)
plot(model2)

nodes(model2,2)

###
predict5 <- predict(model2,train1)
predict6 <- predict(model2,test1)
mean(predict5==train1$Species)
###96
mean(predict6==test1$Species)
###93

library(caret)
confusionMatrix(predict5,train1$Species)
##96
confusionMatrix(predict6,test1$Species)
##93

library(gmodels)
CrossTable(predict5,train1$Species)
##100% accurate foe setosa & virginica
CrossTable(predict6,test1$Species)
##100% accurate for setosa 
### so this is also  a good model

set.seed(123)
sample_data2 <- sample(1:nrow(iris),(nrow(iris)*.6))
train2 <- iris[sample_data2,]
test2 <- iris[-sample_data2,]
###model building
##model1 <- ctree(train$Species~.,data = train,controls = ctree_control(maxsurrogate = 0))

model3<- ctree(train2$Species~.,data = train2)
model3
summary(model3)
plot(model3)

nodes(model3,3)

###
predict7 <- predict(model3,train2)
predict8 <- predict(model3,test2)
mean(predict7==train2$Species)
###96
mean(predict8==test2$Species)
###95

library(caret)
confusionMatrix(predict7,train2$Species)
##96
confusionMatrix(predict8,test2$Species)
##95

library(gmodels)
CrossTable(predict7,train2$Species)
##100% accurate foe setosa & virginica
CrossTable(predict8,test2$Species)
##100% accurate for setosa 
### so this is a good model

##we will consider model 3 bcz diffrence between train and test acciracy is less 