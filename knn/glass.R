##dependent variable is type
## build model for type

glass <- read.csv(file.choose())
View(glass)
table(glass$Type)


summary(glass)
#glass$Type <- int(glass$Type)
str(glass)
#glass$Type <- factor(glass$Type,levels =c("1","2","3","5","6","7"),labels =c("1","2"))


round(prop.table(table(glass$Type))*100,10)

##normalize the data

normalize <- scale(glass)
normal <- as.data.frame(normalize)


####or
normalize <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

normal <- as.data.frame(lapply(glass[,1:9],normalize))


set.seed(123)
#random selection of 70% data.
sampledata <- sample(1:nrow(normal),size=nrow(normal)*0.7,replace = FALSE) 
View(sampledata)
glass_taini <- glass[sampledata,]##70%
glass_test <-  glass[-sampledata,]#30%

#### labels
glass_label <- glass[sampledata,10]
glass_label_test <- glass[-sampledata,10]

table(glass_label_test)    
### model building

library(class)
sqrt(214)=14.62/2=7 or 8
#build two model for 14 and 15

i=1                          # declaration to initiate for loop
j=1                     # declaration to initiate for loop
for (i in 1:28){ 
  predict1 <-  knn(train=glass_taini, test=glass_test, cl=glass_label, k=i)
  j[i] <- 100 * sum(glass_label_test == predict1)/NROW(glass_label_test)
    k=i  
  cat(k,'=',j[i],'\n')       # to print % accuracy 
}


model1 <- knn(glass_taini,glass_test,cl=glass_label,k=1)
plot(model1)
summary(model1)
###accuracy
library(gmodels)
cross<- CrossTable(model1,glass_label_test,prop.chisq = FALSE)

##accuracy <- 100*sum(glass_label_test=model1)/NROW(glass_label_test)
table(model1,glass_label_test)
library(caret)
confusionMatrix(table(model1,glass_label_test))
##87%
model2 <- knn(glass_taini,glass_test,cl=glass_label,k=15)
###accuracy
cross<- CrossTable(model2,glass_label_test)
table(model2,glass_label_test)

confusionMatrix(table(model2,glass_label_test))
###86
model3 <- knn(glass_taini,glass_test,cl=glass_label,k=16)
cross<- CrossTable(model3,glass_label_test)
table(model3,glass_label_test)
confusionMatrix(table(model3,glass_label_test))
#84

model4 <- knn(glass_taini,glass_test,cl=glass_label,k=6)
cross<- CrossTable(model4,glass_label_test)
table(model4,glass_label_test)
confusionMatrix(table(model4,glass_label_test))
#92.3

model5 <- knn(glass_taini,glass_test,cl=glass_label,k=7)
cross<- CrossTable(model5,glass_label_test)

table(model5,glass_label_test)

confusionMatrix(table(model5,glass_label_test))
##93.8
model6 <- knn(glass_taini,glass_test,cl=glass_label,k=8)
cross<- CrossTable(model6,glass_label_test)
table(model6,glass_label_test)

confusionMatrix(table(model6,glass_label_test))

##93.85
model7 <- knn(glass_taini,glass_test,cl=glass_label,k=9)
cross<- CrossTable(model7,glass_label_test)
table(model7,glass_label_test)

confusionMatrix(table(model7,glass_label_test))
##90


####  model 5 &6 is best model

