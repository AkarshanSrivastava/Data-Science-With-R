### in this we have to recom. the car according to address 
### so we dont have need of other clm 
## so we will take only student name and addrs and distance from school

recom <- read.csv(file.choose())
View(recom)


### normalization 
## we know that  there is categorical data and numeric data so we will 
### take only numeric data for normalization
normal <- scale(recom[,3])
View(normal)
## we get normalize value 

#### calculate the  euclidean distance  
## euclidean distance mean it check the distance with all variable 
## distance mean  simillarity 
all(is.na(recom))
distance <- dist(normal,method = "euclidean")
dista <- as.matrix(distance)

### model building
model1 <- hclust(distance,method = "single")
summary(model1)

group <- cutree(model1,4)
car_serialnumber <- as.matrix(group)
recom <- cbind(recom,car_serialnumber)
View(recom)
aggregate(recom[,-c(1,2,3)],by=list(car_serialnumber),FUN = mean)
getwd()
setwd("C:/Users/Acer/Desktop/Assigment/CLUSTRING")
write.csv(recom,file = "final_Result.csv")
