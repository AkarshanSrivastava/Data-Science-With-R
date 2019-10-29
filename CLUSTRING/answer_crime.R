crime <- read.csv(file.choose())
View(crime)
### here we saw that there is only 5 clm so we will go with hierarchical clustring

###normalize the data
normal <- scale(crime[,2:5])
View(normal)


#### calculate the distance
distance <- dist(normal,method="euclidean")
distance
class(distance)

## now we have distance so we will build our model
## model Building
model1 <- hclust(distance,method = "single")
View(model1)
summary(model1)


###dendogram
plot(model1)
plot(model1,hang=-1)


###cutree

rect.hclust(model1,k=4,border = "Red")


##gruop of cluster

group <- cutree(model1,k=4)
View(group)
r<- as.matrix(group)
View(r)


### add in the orig data set
crime <- cbind(crime,r)
View(crime)

#### with Maximum, Manhattan, Canberra, binary, Minowski distance
#### calculate the distance
distance <- dist(normal,method="Manhattan")
distance
class(distance)

## now we have distance so we will build our model
## model Building
model1 <- hclust(distance,method = "single")
View(model1)
summary(model1)


###dendogram
plot(model1)
plot(model1,hang=-1)


###cutree

rect.hclust(model1,k=4,border = "Red")


##gruop of cluster

group <- cutree(model1,k=4)
View(group)
r<- as.matrix(group)
View(r)


### add in the orig data set
crime <- cbind(crime,r)
View(crime)
distance <- dist(normal,method="canberra")
distance
class(distance)

## now we have distance so we will build our model
## model Building
model1 <- hclust(distance,method = "single")
View(model1)
summary(model1)


###dendogram
plot(model1)
plot(model1,hang=-1)


###cutree

rect.hclust(model1,k=4,border = "Red")


##gruop of cluster

group <- cutree(model1,k=4)
View(group)
r<- as.matrix(group)
View(r)


### add in the orig data set
crime <- cbind(crime,r)
View(crime)

distance <- dist(normal,method="binary")
distance
class(distance)

## now we have distance so we will build our model
## model Building
model1 <- hclust(distance,method = "single")
View(model1)
summary(model1)


###dendogram
plot(model1)
plot(model1,hang=-1)


###cutree

rect.hclust(model1,k=4,border = "Red")


##gruop of cluster

group <- cutree(model1,k=4)
View(group)
r<- as.matrix(group)
View(r)


### add in the orig data set
crime <- cbind(crime,r)
View(crime)


### add in the orig data set
crime <- cbind(crime,r)
View(crime)


##save the data
getwd()
setwd("C://Users//Acer//Desktop//Assigment//CLUSTRING")
write.csv(crime,file = "final_crime.csv")
aggregate(crime[,-1],by=list(crime$X),mean)

