#### we find the price
toyota <- read.csv(file.choose())
View(toyota)
### categorical data 
##fuel type , color 
## coverting in to dummy
attach(toyota)
library(dummies)
dummy0 <- as.data.frame(dummy(Fuel_Type))

## discard the elemenet id , model ,color

toyota1 <- toyota[,-c(1,2,4,8,10,14,15,16,11,17,18,19,20,21,38,37,36.35,34,33,32,28)]
View(toyota1)
toyota0 <- cbind(toyota1,dummy0)

View(toyota0)
all(is.na(toyota0))
##no NA 
pairs(toyota0)
cor(toyota0)


##Dependency

library(corpcor)
cor2pcor(cor(toyota0))
attach(toyota0)

## model building 
model1 <- lm(toyota0$Price~.,data = toyota0)
summary(model1)

model2 <- lm(toyota0$Price~Mfg_Month+Mfg_Year+KM+HP+Automatic+(cc)^2+ABS
             +(Airbag_1)^2+(Airbag_2)^2+Airco+Automatic_airco+Boardcomputer+(Central_Lock)^2
             +Powered_Windows+(Power_Steering)^2)
summary(model2)


library(car)
influencePlot(model2)
###81 is outlier
influence.measures(model2)
influenceIndexPlot(model2)


model3 <- lm(Price~.,data = toyota0[,c(-81)])
summary(model3)
avPlots(model3)


#### after the concert with domain expert and business objective and subject expert
model4 <- lm(Price~.-Airbag_1-Airbag_2-Central_Lock 
          -`dummy(Fuel_Type)Petrol`  -Power_Steering -cc -`dummy(Fuel_Type)CNG`,data = toyota0)
summary(model4)


plot(model4)
