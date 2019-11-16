library(readr)
cal <- read.csv("C://Akarshan's Document//Akarshan//excelrDATASCIENCE//Data Science Assigment//Dataset//SIMPLE LINEAR REGRESSION//calories_consumed.csv")
View(cal)

as.data.frame(cal)

names(cal)[1] <- paste("wt.gained")
names(cal)[2] <- paste("cal.cons")

##EDA
sumary(cal)
boxplot(wt.gained) ## no outlier
boxplot(cal.cons)  ## no outlier

## 1st model

attach(cal)
plot(cal.cons,wt.gained)
cor(cal.cons,wt.gained)

## simple linear regression model building

reg1 <- lm(wt.gained~cal.cons)
summary(reg1)

confint(reg1, level = 0.95)
predict1 <- reg1$fitted.values
predict1

errors1 <- reg1$residuals
errors1

rmse1 <- sqrt(sum(reg1$residuals^2)/nrow(cal))
rmse1

cal1 <- cbind(cal,predict1,errors1)
View(cal1)



## 2nd Model
plot(log(cal.cons),wt.gained)
cor(log(cal.cons),wt.gained)

reg2 <- lm(wt.gained~log(cal.cons))
summary(reg2)

confint(reg2, level = 0.95)
predict2 <- reg2$fitted.values
predict2

error2 <- reg2$residuals
error2

rmse2 <- sqrt(sum(reg2$residuals^2)/nrow(cal))
rmse2

cal2 <- cbind(cal,predict2,error2)
View(cal2)



## 3rd Model 

plot(cal.cons,log(wt.gained))
cor(cal.cons,log(wt.gained))

reg3 <- lm(log(wt.gained)~cal.cons)
summary(reg3)

confint(reg3, level = 0.95)

predict3 <- exp(reg3$fitted.values)
predict3


error3 <- wt.gained-predict3
error3

rmse3 <- sqrt(sum(error3^2)/nrow(cal))
rmse3

cal3 <- cbind(cal,predict3,error3)
View(cal3)



## 4th model
plot(log(cal.cons),log(wt.gained))
cor(log(cal.cons),log(wt.gained))

reg4 <- lm(log(wt.gained)~log(cal.cons))
summary(reg4)

confint(reg4, level = 0.95)

predict4 <- exp(reg4$fitted.values)
predict4


error4 <- wt.gained-predict4
error4

rmse4 <- sqrt(sum((error4)^2)/nrow(cal))
rmse4

cal4 <- cbind(cal,predict4,error4)
View(cal4)

## 5th Model

plot((cal.cons)^4,wt.gained)
cor((cal.cons)^4,wt.gained)

reg5 <- lm(wt.gained~(cal.cons)^4)
summary(reg5)

confint(reg5 ,level = 0.95)

predict5 <- reg5$fitted.values
predict5


error5 <- wt.gained-predict5
error5

rmse5 <- sqrt(sum((error5)^2)/nrow(cal))
rmse5

cal5 <- cbind(cal,predict5,error5)
View(cal5)
