##### predict the price

computer <- read.csv(file.choose())
View(computer)
###so we see that there is some categorical data so we will create it as dummy
library(dummies)
attach(computer)
dummy1 <- as.data.frame(dummy(cd))
dummy2 <- as.data.frame(dummy(multi))
dummy3 <- as.data.frame(dummy(premium))
dummy0 <- cbind(dummy1,dummy2,dummy3)
View(dummy0)              
### dont need clm 1 so just remove it 
computer1<- computer[,-c(1,7,8,9)]
computer0 <- cbind(computer1,dummy0)
View(computer0)


#######EDA
attach(computer0)
c(mean(price),mean(speed),mean(hd),mean(ram),mean(screen),mean(ads),
           mean(trend),mean(`dummy(cd)no`),mean(`dummy(cd)yes`),
           mean(`dummy(multi)no`),mean(`dummy(multi)yes`),mean(`dummy(premium)no`
                                                               ,mean(`dummy(premium)yes`)))

###219.5766097   52.0110241  416.6016936    8.2869468   14.6087234
## 221.3010066   15.9269851    0.5353890    0.4646110    0.8605208
###  0.1394792    0.0000000
c(median(price),median(speed),median(hd),median(ram),median(screen),median(ads),
  median(trend),median(`dummy(cd)no`),median(`dummy(cd)yes`),
  median(`dummy(multi)no`),median(`dummy(multi)yes`),median(`dummy(premium)no`
                                                      ,median(`dummy(premium)yes`)))
###2144   50  340    8   14  246   16    1    0    1    0    0


######Full EDA
all(is.na(computer0))
###no NA valu dont need to perform imputation




### correlation check

pairs(computer0)
cor(computer0)
library(corpcor)
cor2pcor(cor(computer0))


#### build model

model1 <- lm(log(price)~.,data = computer0)
summary(model1)
##### all are significant 

predict1<- exp(model1$fitted.values)
plot(model1)
error <- computer0$price-predict1

##RMS

### it define the error in  between actaul and between 
sqrt(mean(model1$residuals^2))
rms <- ((sqrt(error))/(nrow(computer0)))
sqrt(sum(error^2)/nrow(computer))


###rms is high so we will build more model and find min rms value