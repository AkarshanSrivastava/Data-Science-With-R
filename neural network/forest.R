library(Amelia)
library(psych)
library(neuralnet)
library(ie2misc)
library(caret)
library(gmodels)

forest <- read.csv("C://Users//Acer//Desktop//Assignment//pending//neural network//forestfires.csv")
forest <- forest[,-c(1,2)]
missmap(forest)
##0% missing value 
describe(forest)
##value is to mch high so we will do normalization

forest$size_category <-ifelse(forest$size_category=="small",yes=0,no=1) 
normal <- as.data.frame(scale(forest[,c(-29)]))
normal <- cbind(normal,forest[,c(29)])
colnames(normal)[29] <- "size"
str(normal)
describe(normal)
## we got 1st 2nd 3rd 4th BM
summary(normal)

missmap(normal)
##0% missing value 

### visualization

c <- ggplot(data = forest,aes(x=forest$FFMC,fill=forest$size_category,group=1))+geom_histogram()
e <-c+theme(panel.background = element_rect(fill = "skyblue"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "REd"))
h<- g+facet_grid(~forest$size_category)
h


### ffmc is high in small area 



c <- ggplot(data = forest,aes(x=forest$DC,fill=forest$size_category,group=1))+geom_histogram()
e <-c+theme(panel.background = element_rect(fill = "skyblue"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "REd"))
h<- g+facet_grid(~forest$size_category)
h

##dc is high in small area


c <- ggplot(data = forest,aes(x=forest$ISI,fill=forest$size_category,group=1))+geom_histogram()
e <-c+theme(panel.background = element_rect(fill = "skyblue"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "REd"))
h<- g+facet_grid(~forest$size_category)
h


## ISI is also high in small Area


c <- ggplot(data = forest,aes(x=forest$temp,fill=forest$size_category,group=1))+geom_histogram()
e <-c+theme(panel.background = element_rect(fill = "skyblue"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "REd"))
h<- g+facet_grid(~forest$size_category)
h

### temp is also high in small area


c <- ggplot(data = forest,aes(x=forest$RH,fill=forest$size_category,group=1))+geom_histogram()
e <-c+theme(panel.background = element_rect(fill = "skyblue"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "REd"))
h<- g+facet_grid(~forest$size_category)
h

##RH is high in smakk area 

c <- ggplot(data = forest,aes(x=forest$wind,group=1,col="red"))+geom_bar(fill="yellow")
e <-c+theme(panel.background = element_rect(fill = "skyblue"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "REd"))
g

###we can say that most of the wind lie between 2.5 to 5.5



c <- ggplot(data = forest,aes(x=forest$rain,group=1))+geom_bar()
e <-c+theme(panel.background = element_rect(fill = "skyblue"))
f <- e+theme(plot.title = element_text(hjust = .06,face = "bold"))
g <- f+theme(plot.title = element_text(hjust = .06,face = "bold",colour = "REd"))
h <-g+facet_grid(~forest$size_category) 
h
####area is more in large 



set.seed(222)
sample_data <- sample(nrow(normal),(nrow(normal)*.8))
train <- normal[sample_data,]
test <-  normal[-sample_data,]


###model building 
model1 <- neuralnet(train$size~.,data = train,hidden = 4)

model1
summary(model1)
model1$result.matrix
plot(model1)
#ERROR 3.472934e-02
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(model1)
str(model1)
pred0 <- predict(model1,test)
pred00 <- compute(model1,test)
pred00
pred00$net.result
pred00$neurons
model1$net.result
## neurons for info 
library(gmodels)
pre <-ifelse(pred00$net.result>.5,yes="0",no="1")
CrossTable(pre,test$size)
library(caret)
library(ModelMetrics)

rmse(as.numeric(pre),test$size)
##.975
cor(as.numeric(pre),test$size)
##srong core.
model2 <- neuralnet(train$size~.,data = train,hidden = c(4,2))

model2
summary(model2)
model2$result.matrix
plot(model2)

##error 5.285913e-01

pred0 <- predict(model2,test)
pred00 <- compute(model2,test)
pred00
pred00$net.result
pred00$neurons
model2$net.result
## neurons for info 
pre <-ifelse(pred00$net.result>.5,yes="0",no="1")
CrossTable(pre,test$size)
library(caret)
library(ModelMetrics)

rmse(as.numeric(pre),test$size)
##.970
cor(as.numeric(pre),test$size)
##strong core.

model3 <- neuralnet(train$size~.,data = train,hidden = c(4,3,2))

model3
summary(model3)
model3$result.matrix
plot(model3)
#0.001424694



pred0 <- predict(model3,test)
pred00 <- compute(model3,test)
pred00
pred00$net.result
pred00$neurons
model3$net.result
## neurons for info 
pre <-ifelse(pred00$net.result>.5,yes="0",no="1")
CrossTable(pre,test$size)

rmse(as.numeric(pre),test$size)
##.970
cor(as.numeric(pre),test$size)
##strong core.



### according to me least error value is best model we will take model 3 for future use 
