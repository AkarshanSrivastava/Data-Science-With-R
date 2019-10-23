company <- read.csv("C://Users//Acer//Desktop//Assignment//pending//random forest//Company_Data.csv")
View(company)
#############EDA###############
median(company$Sales)
##7.49

c <- as.data.frame(ifelse(company$Sales>=7.49,yes = "High",no="Low"))
company <- cbind(c,company)
View(company)
colnames(company)[1] <- "sales"
View(company)
company <- company[,-c(2)]
View(company)
library(psych)
describe(company)
## 1 2 3 4 th BM
library(Amelia)
missmap(company)
### 0% missing data
####GRAPH
library(ggplot2)
c <- ggplot(data = company,aes(x=company$CompPrice,fill=company$sales))+geom_histogram()
c
d <- c+theme(panel.background = element_rect(fill = "BROWN"))
e <- d+theme(plot.title = element_text(hjust = .06,face = "bold"))
e <- d+theme(plot.title = element_text(hjust = .05,face = "bold",colour = "Black"))
e



###when computer price is 75 or 175 then there is higher chance of sales 
### if it is between 110 to 140 so there is higher chances of no sales 

c <- ggplot(data=company, aes(company$Income, col= company$sales)) +
  geom_freqpoly(binwidth = 1) + labs(title="sales wrt to income")

d <- c+theme(panel.background = element_rect(fill = "white"))
e <- d+theme(plot.title = element_text(hjust = .06,face = "bold"))
e <- d+theme(plot.title = element_text(hjust = .05,face = "bold",colour = "Black"))
e

#### if income between 25 to 60 so its mean there is min sales (mean no sales)
## but when income increase then sales increase


p <- ggplot(data=company, aes(company$Advertising, y= company$sales,shape="popular")) +
  geom_count(col="blue") + labs(title="sales by advertising ")
p+theme_bw()
##num of count wrt to advertising

c <- ggplot(data=company, aes(company$Population, fill=company$sales)) +
  geom_histogram() + labs(title="sales wrt to population")
c + theme_bw()



c <- ggplot(data=company, aes(x=company$Education,fill=company$sales)) +
  geom_bar() + labs(title="sales wrt to education")
d <- c+theme(panel.background = element_rect(fill = "yellow"))
e <- d+theme(plot.title = element_text(hjust = .06,face = "bold"))
e <- d+theme(plot.title = element_text(hjust = .05,face = "bold",colour = "Black"))
e


c <- ggplot(data=company, aes(x=company$ShelveLoc,fill=company$sales)) +
  geom_bar() + 
  facet_grid(.~sales)+labs(title="sales wrt bad good mediu ")
d <- c+theme(panel.background = element_rect(fill = "skyblue"))
e <- d+theme(plot.title = element_text(hjust = .06,face = "bold"))
e <- d+theme(plot.title = element_text(hjust = .05,face = "bold",colour = "Black"))
e

c <- ggplot(data=company, aes(x=company$Age,fill=company$sales)) +
  geom_bar() + 
  facet_grid(.~sales)+labs(title="sales wrt to age")
d <- c+theme(panel.background = element_rect(fill = "black"))
e <- d+theme(plot.title = element_text(hjust = .06,face = "bold"))
e <- d+theme(plot.title = element_text(hjust = .05,face = "bold",colour = "Black"))
e



c <- ggplot(data=company, aes(x=company$Income,fill=sales )) +
  geom_bar() + 
  facet_grid(.~sales)+labs(title="workclass by salary")
d <- c+theme(panel.background = element_rect(fill = "pink"))
e <- d+theme(plot.title = element_text(hjust = .06,face = "bold"))
e <- d+theme(plot.title = element_text(hjust = .05,face = "bold",colour = "Black"))
e

##spliting the data
set.seed(123)
sample_data <- sample(1:nrow(company),(.07*nrow(company)),replace = FALSE)
test <- company[sample_data,]
train <- company[-sample_data,]
install.packages("randomForest")
library(randomForest)
bestmtry <- tuneRF(company,company$sales,stepFactor = 1.2,prove = .002,trace = T,plot = T)
##  by this we will get better accuracy 
###
#####model building
model1 <- randomForest(train$sales~.,data = train,na.action =na.roughfix,importance=TRUE)
model1
summary(model1)
print(model1)
model1$importance
importance(model1)
library(caret)
varImpPlot(model1)
##price is highest information gain then shelveloc then advertising,least is us 


predict1 <- predict(model1,train)
predict2 <- predict(model1,test)
mean(predict1==train$sales)
mean(predict2==test$sales)
##82%
library(caret)
confusionMatrix(predict1,train$sales)
##100% accuracy
confusionMatrix(predict2,test$sales)
##82
##overfit pro 
##so we will do boosting
boos <- randomForest(train$sales~.,data = train,na.action = na.roughfix,trails=10,importance=TRUE)
boos1<- predict(boos,train)
boos2 <- predict(boos,test)
mean(boos1==train$sales)###100%
mean(boos2==test$sales)###86## 
## SO IT IS A GOOD MODEL

