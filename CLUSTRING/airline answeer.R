###################### hierarchical clustering###########
#install.packages("xlsx")
#install.packages(c("doParallel", "kselection"))
library(xlsx)

airline <- readxl::read_excel(file.choose(),2)
View(airline)
summary(airline)


normal <- scale(airline[,c(-1)])
View(normal)

distance <- dist(normal,method = "euclidean")

model1 <- hclust(distance,method = "single")
summary(model1)

plot(model1)

rect.hclust(model1,k=10,border = "red")
group <- cutree(model1,10)
View(group)

cluster <- as.matrix(group)
airline<- cbind(airline,cluster)
View(airline)
aggregate(airline[,c(-1)],by=list(airline$cluster),FUN = mean)
getwd()

setwd("C:/Users/Acer/Desktop/Assigment/CLUSTRING")
write.csv(airline,file = "cluster_airline.csv")


### euclidean not for negative correlation 
####with diffrent distance
distance1 <- dist(normal,method = "manhattan")

model5 <- hclust(distance1,method = "single")
summary(model5)

plot(model5)

rect.hclust(model5,k=10,border = "red")
group <- cutree(model5,10)
View(group)

cluster <- as.matrix(group)
airline<- cbind(airline,cluster)
View(airline)
aggregate(airline[,c(-1)],by=list(airline$cluster),FUN = mean)



##### with Maximum, Canberra, binary, Minowski
distance1 <- dist(normal,method = "Maximum")

model5 <- hclust(distance1,method = "single")
summary(model5)

plot(model5)

rect.hclust(model5,k=10,border = "red")
group <- cutree(model5,10)
View(group)

cluster <- as.matrix(group)
airline<- cbind(airline,cluster)
View(airline)
aggregate(airline[,c(-1)],by=list(airline$cluster),FUN = mean)

distance1 <- dist(normal,method = "Canberra")

model5 <- hclust(distance1,method = "single")
summary(model5)

plot(model5)

rect.hclust(model5,k=10,border = "red")
group <- cutree(model5,10)
View(group)

cluster <- as.matrix(group)
airline<- cbind(airline,cluster)
View(airline)
aggregate(airline[,c(-1)],by=list(airline$cluster),FUN = mean)

distance1 <- dist(normal,method = "binary")

model5 <- hclust(distance1,method = "single")
summary(model5)

plot(model5)

rect.hclust(model5,k=10,border = "red")
group <- cutree(model5,10)
View(group)

cluster <- as.matrix(group)
airline<- cbind(airline,cluster)
View(airline)
aggregate(airline[,c(-1)],by=list(airline$cluster),FUN = mean)


####### hierarchical Clustring completed  ##########################

#################   K-MEAN  #################



library(xlsx)
airline2 <- readxl::read_xlsx(file.choose(),2)
View(airline2)

normalized <- scale(airline2[,2:12])
####find the best clusternumber####
##elbo curve
((sqrt(3999))/2)
###31.61  so 31 or 32

############### SCREE PLOT #################

wss=  (nrow(normalized)-1)*sum(apply(normalized, 2,var))
# Determine number of clusters by scree-plot 
for (i in 2:12) wss[i] = sum(kmeans(normalized, centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
# Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
####  8 cluster  ##### 


###k selection####
library(kselection)
kselection(normalized)
##f(k) finds 8 clusters


library(doParallel)
registerDoParallel(cores=2) ####computer perfo  cpu
k <- kselection(iris[,-5], parallel = TRUE, k_threshold = 0.9, max_centers=12)



#### so ow i will  take k selection and build model with 8 cluster

modelk <- kmeans(normalized,8)
View(modelk)
#####K-means clustering with 8 clusters of sizes 1649, 135, 55, 15, 587, 28, 702, 828

cluster_k <- as.matrix(modelk$cluster)
View(cluster_k)


final_Result <- cbind(airline2,cluster_k)
View(final_Result)
 
aggregate(final_Result,by=list(final_Result$cluster_k),FUN = mean)

############## k Mean clustring completed #################

