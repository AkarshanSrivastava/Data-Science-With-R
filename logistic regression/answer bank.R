## find y
Bank <- read.csv(file.choose(),sep = ";")
View(Bank)
### first we covert y in to 0 and 1

## so we will use cut or ifelse function

y <- ifelse(Bank$y =="yes",1,0)
table(y)
y<- as.data.frame(y)
#######EDA.
##Manipulation.
## remove contact(9),day(10),month(11) compaign(13),pdays(14) previous(15) poutcome(16)

Bank <- Bank[,-c(9,11,10,13,14,15,16)]


### check for NA value 
all(is.na(Bank))
## so there is no NA value so dont need to do imputation 

### but in this model there is some Categorical data so 
##first we need to convert it into dummy data 
library(dummies)
attach(Bank)
dummy1 <- as.data.frame(dummy(job))
dummy2 <- as.data.frame(dummy(marital))
dummy3 <- as.data.frame(dummy(education))
dummy4 <- as.data.frame(dummy(default))
dummy5 <- as.data.frame(dummy(housing))
dummy6 <- as.data.frame(dummy(loan))



##contrasts(y2)  #### checking the dummy value 
Bank <- Bank[,-c(2,3,4,5,7,8,10)]
View(Bank)
Bank <- cbind(Bank,dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,y)
View(Bank)

##1ST BM
mean <- c(mean(Bank$age),mean(Bank$balance),mean(Bank$duration))

median <- c(median(Bank$age),median(Bank$balance),median(Bank$duration))

mode <- c(sort(table(age)))

#2ND BM

library(moments)
variance <- c(var(Bank$age),var(Bank$balance),var(Bank$duration))

standard_deviation <- c(sd(Bank$age),sd(Bank$balance),sd(Bank$duration))


#3RD BM

skewness <- c( skewness(Bank$age), skewness(Bank$balance), skewness(Bank$duration))


###4th BM

kurtosis <- c( kurtosis(Bank$age), kurtosis(Bank$balance), kurtosis(Bank$duration))

####graphical tech

barplot(age,balance,duration)
boxplot(age,balance,duration)
hist(age)
hist(balance)
hist(duration)
#####  model building
str(Bank)
model1 <-glm(y~.,data = Bank,family = binomial) 
summary(model1)
#### not getting significant p value in colomn age ,job ,employed.etc

###so apply ^2 transformation

model2 <- glm(y^2~.,data = Bank,family = binomial)
summary(model2)
#### not find significant p value 
model3 <- glm(y^3~.,data = Bank,family = binomial)
summary(model3)
## not getting significant p value 
##step(model3)
### so now we check input side
model4 <- glm(Bank$y~Bank$age^3+Bank$balance^3+Bank$duration^3+Bank$`dummy(job)admin.`^3+Bank$`dummy(job)blue-collar`^3+Bank$`dummy(job)entrepreneur`^3+Bank$`dummy(job)housemaid`^3+Bank$`dummy(job)management`^3+Bank$`dummy(job)retired`^3+Bank$`dummy(job)self-employed`^3+Bank$`dummy(job)services`^3+Bank$`dummy(job)student`^3+Bank$`dummy(job)technician`^3+Bank$`dummy(job)unemployed`^3+Bank$`dummy(job)unknown`+Bank$`dummy(marital)divorced`^3+Bank$`dummy(marital)married`^3+Bank$`dummy(marital)single`^3+Bank$`dummy(education)primary`^3+Bank$`dummy(education)secondary`^3+Bank$`dummy(education)tertiary`^3+Bank$`dummy(education)unknown`^3+Bank$`dummy(default)no`^3+Bank$`dummy(default)yes`^3+Bank$`dummy(housing)no`^3+Bank$`dummy(housing)yes`^3+Bank$`dummy(loan)no`^3+Bank$`dummy(loan)yes`^3,data = Bank,family = binomial)
summary(model4)
#### do not find p value significant 
## so its mean remove one row clm 

cor(Bank)
library(corpcor)
cor2pcor(cor(Bank))

library(car)
influencePlot(model1)
influenceIndexPlot(model1)
influence.measures(model1)
###outlier 24149,39990 so we will remove this row and build our model
model5 <- glm(Bank$y~.,data = Bank[-c(24149,39990),])
summary(model5)
###not find p as a significant

avPlots(model5)
###### so now we will meet with subject matter expert and domain expert 
### then remove the clm

model6 <- glm(Bank$y~.-Bank$age,data = Bank,family = binomial)
summary(model6)
### we find p is significant 

step(model6,trace = 1,k=2)
attach(Bank)
model7 <- glm(Bank$y ~ age + balance + duration + `dummy(job)admin.` + `dummy(job)management` + 
                `dummy(job)retired` + `dummy(job)services` + `dummy(job)student` + 
                `dummy(job)technician` + `dummy(job)unemployed` + `dummy(marital)divorced` + 
                `dummy(marital)married` + `dummy(education)primary` + `dummy(education)tertiary` + 
                `dummy(default)no` + `dummy(housing)no` + `dummy(loan)no`,data = Bank,family = binomial)
summary(model7)


##p is significant and least AIC value 

pre<- model7$fitted.values
pre
actual <- Bank$y
error=pre-actual
##root mean Sqaure
rms <- sqrt((sum(error)^2)/nrow(Bank))
rms
### now we will find confusion matrix

confusion <- table(pre>.5,Bank$y)
confusion

accuracy <- sum(diag(confusion)/sum(confusion))
accuracy
####88% accurate model

predictvalue <- NULL
yesno <- NULL


predictvalue <- ifelse(pre>.05,1,0)
yesno <- ifelse(pre>.05,"yes","no")


####add a new clm to store these value

Bank[,"prob"] <- pre
Bank[,"predictvalue"] <- predictvalue
Bank[,"yes_no"] <- yesno
View(Bank)
#######ROCR ###

library(ROCR)
roc <- prediction(pre,Bank$y)
roc1 <- performance(roc,"tpr","fpr")
str(roc1)


plot(roc1,colorize=T,text.adj=c(-.02,1.7))

##more area in curve mean better model

#####CUtoff value 

rocr_cutoff <- data.frame(cut_off = roc1@alpha.values[[1]],fpr=roc1@x.values,tpr=roc1@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

########################## completed ###################################