
glass <- read.csv("C://Users//Sanchi//Desktop//Assignment//Complete//R//knn//glass.csv")
View(glass)
table(glass$Type)


summary(glass)
str(glass)
glass$Type <- factor(glass$Type,levels =c("1","2","3","5","6","7"),labels =c("1"))


round(prop.table(table(glass$Type))*100,10)

##normalize the data
normalize <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

normal <- as.data.frame(lapply(glass[,1:9],normalize))


set.seed(123)
#random selection of 70% data.
sampledata <- sample(1:nrow(normal),size=nrow(normal)*0.8,replace = FALSE) 
View(sampledata)
glass_taini <- glass[sampledata,]##70%
glass_test <-  glass[-sampledata,]#30%

#### labels
glass_label <- glass[sampledata,10]
glass_label_test <- glass[-sampledata,10]

table(glass_label_test)    
### model building

library(class)

4i=1                          # declaration to initiate for loop
j=1                     # declaration to initiate for loop
for (i in 1:15){ 
  predict1 <-  knn(train=glass_taini, test=glass_test, cl=glass_label, k=i)
  j[i] <- 100 * sum(glass_label_test == predict1)/NROW(glass_label_test)
  k=i  
  cat(k,'=',j[i],'\n')       # to print % accuracy 
}


model1 <- knn(glass_taini,glass_test,cl=glass_label,k=3)
plot(model1)
summary(model1)
###accuracy
library(gmodels)
library(caret)
cross<- CrossTable(model1,glass_label_test,prop.chisq = FALSE)
###100*sum(glass_label_test=model1)/NROW(glass_label_test)
table(model1,glass_label_test)
library(caret)
confusionMatrix(table(model1,glass_label_test))
##97