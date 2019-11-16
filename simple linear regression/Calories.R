#####simple Logistic reg dependent(y) and independent(x) both are cont.

calories<- read.csv(file.choose())
View(calories)
summary(calories)
##in mean and median there is to much diff so there is a possibility of outlier 
############EDA###############

attach(calories)
mean1<- c(mean(Weight.gained..grams.),mean(Calories.Consumed))
mean1
357.7143 2340.7143
c(median(Weight.gained..grams.),median(Calories.Consumed))
200 225

sort(c(table(calories$Weight.gained..grams.),table(calories$Calories.Consumed)))
200 1900 

##there is to much diff so there is a chance of outlier


#####2nd BM######

c(var(Weight.gained..grams.),var(Calories.Consumed))
111350.7 565668.7

c(sd(Weight.gained..grams.),sd(Calories.Consumed))

333.6925 752.1095

###3rd BM

library(moments)
c(skewness(Weight.gained..grams.),skewness(Calories.Consumed))
1.1169767 0.5825597


####4th BM
c(kurtosis(Weight.gained..grams.),kurtosis(Calories.Consumed))
2.891938 2.403367


###Graph


hist(Weight.gained..grams.)#######tails in right concentrate on right  mask in left + skewness
hist(Calories.Consumed)    ####### symmatric 

barplot(Weight.gained..grams.)
barplot(Calories.Consumed)
barplot(Weight.gained..grams.,Calories.Consumed)

boxplot(Weight.gained..grams.,Calories.Consumed)

qqnorm(Weight.gained..grams.)
qqline(Weight.gained..grams.)
qqnorm(Calories.Consumed)
qqline(Calories.Consumed)


############normality check

shapiro.test(Weight.gained..grams.) ######not normal
shapiro.test(Calories.Consumed)#######not normal




summary(calories)

core <- cor(Calories.Consumed,Weight.gained..grams.)
core
.9469   ###strong corelation

plot(Calories.Consumed,Weight.gained..grams.)


linearirty <- lm(Weight.gained..grams.~Calories.Consumed)
summary(linearirty)
Bo=-625.75236
B1=.42
P<.005
R^2 .89
Adj R^2 .88

-625.75+.42*(1500)
confint(calories,level=.95)
predict(linearirty,prdict="predict")
linearirty$fitted.values

##1st predict value 4.482599 -	108  =-103.517
#############Error is to much high 


####apply transformation
cor(log(Calories.Consumed),log(Weight.gained..grams.))
.92

model1 <- lm(log(Weight.gained..grams.)~log(Calories.Consumed))
summary(model1)
model1$fitted.values
log(calories$Weight.gained..grams.)

cor(log(Weight.gained..grams.),log(Calories.Consumed))
 ####rms value

########
4.43-4.68
##Error =predict- actual