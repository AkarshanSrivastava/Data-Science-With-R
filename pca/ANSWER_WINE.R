library(readr)
wine <- read.csv(file.choose())
View(wine)
#w <- wine[,c(-1)]

#### go for PCA
pca <- princomp(wine,cor = TRUE,scores = TRUE,covmat = NULL)
str(pca)
summary(pca)
loadings(pca) ##weight
#### according to qus we will take first 3 that give  67 % info

biplot(pca)
plot(pca)
pca$scores
pca$loadings
top_pca <- pca$scores[,1:3]

#### bind

wine <- cbind(wine,top_pca)
##now we will perform heirarchical clustring#####

clus <- wine[,15:17]
normal <- scale(clus)
distance <- dist(normal)
##model build

model1 <- hclust(distance,method="single")
summary(model1)

group <- cutree(model1,6)
hierarchical_cluster <- as.matrix(group)
View(hierarchical_cluster)
wine<- cbind(wine,hierarchical_cluster)
aggregate(wine,by=list(wine$hierarchical_cluster),FUN = mean)


##################  k mean  #####


##distance according to elbo curve is

((sqrt(178))/2)
###6 or 7

##scree plot

wss=  (nrow(normal)-1)*sum(apply(normal, 2,var))
# Determine number of clusters by scree-plot 
for (i in 2:8) wss[i] = sum(kmeans(normal, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
# Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
###  5 or 6


###we build model with 6 cluter

model2 <- kmeans(normal,6)
summary(model2)
cluster <- model2$cluster
kmeans_cluster <- as.matrix(cluster)
wine <- cbind(wine,kmeans_cluster)
aggregate(wine,by=list(wine$kmeans_cluster),FUN = mean)

#########################################



wine_orignal <- read.csv(file.choose())

##clustering on original data


##hierarchical clustering

normal2 <- scale(wine_orignal)
distance3 <- dist(normal2)
model3 <- hclust(distance3)
summary(model3)


group2 <- cutree(model3,5)
orig_hier <- as.matrix(group2)
wine_orignal<- cbind(wine_orignal,orig_hier)
aggregate(wine_orignal,by=list(wine_orignal$orig_hier),FUN = mean)


####kmean

##scree  plot

wss=  (nrow(normal2)-1)*sum(apply(normal2, 2,var))
# Determine number of clusters by scree-plot 
for (i in 2:8) wss[i] = sum(kmeans(normal2, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
# Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
### 5 or 6


model4 <- kmeans(normal2,5)
cluster1<- model4$cluster
kmean_orig_clus <- as.matrix(cluster1)
wine_orignal<- cbind(wine_orignal,kmean_orig_clus)
aggregate(wine_orignal,by=list(wine_orignal$kmean_orig_clus),FUN = mean)





##### SO THERE IS  O TO MUCH DIFF IN CLUSTER BUT THE DIFF BETWEEN AVERAGE IS MORE


####IN PCA WWE FIND 6 CLUSTER IS BEST
## IN ORIG. DATA WE FIND 5 IS GOOD