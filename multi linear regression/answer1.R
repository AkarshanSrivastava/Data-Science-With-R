ABC<- read.csv(file.choose())
View(ABC)

###there is a categorical data so we create dummy 

library(dummies)
attach(ABC)

dum <- as.data.frame(dummy(State))
View(dum)
### now we remove state clm from ABC
ABCD <- ABC[,-4]
View(ABCD)

####now we add dum and ABCD in one data frame 
company <- cbind(ABCD,dum)
View(company)
class(company)
#####  now we wprk on company data set

#########EDA#######

##1st BM
attach(company)
c(mean(R.D.Spend),mean(Administration),mean(Marketing.Spend),mean(Profit)
,mean(`dummy(State)California`),mean(`dummy(State)Florida`),mean(`dummy(State)New York`))

73721.62 121344.64 211025.10 112012.64      0.34      0.32      0.34

c(median(R.D.Spend),median(Administration),median(Marketing.Spend),median(Profit)
  ,median(`dummy(State)California`),median(`dummy(State)Florida`),median(`dummy(State)New York`))

73051.08 122699.79 212716.24 107978.19      0.00      0.00      0.00
####mode
sort(table(R.D.Spend))  ----->0
sort(table(Administration))   ---->
sort(table(Marketing.Spend))  ---->0
sort(table(Profit))---->
sort(table(`dummy(State)California`))   ----->0
sort(table(`dummy(State)Florida`))  ----->0
sort(table(`dummy(State)New York`)) ----->0

#####2nd BM


c(var(R.D.Spend),var(Administration),var(Marketing.Spend),var(Profit)
  ,var(`dummy(State)California`),var(`dummy(State)Florida`),var(`dummy(State)New York`))

2.107017e+09 7.849973e+08 1.495492e+10 1.624588e+09 2.289796e-01 2.220408e-01 2.289796e-01

c(sd(R.D.Spend),sd(Administration),sd(Marketing.Spend),sd(Profit)
  ,sd(`dummy(State)California`),sd(`dummy(State)Florida`),sd(`dummy(State)New York`))

4.590226e+04 2.801780e+04 1.222903e+05 4.030618e+04 4.785181e-01 4.712121e-01 4.785181e-01



####3rd BM

library(moments)


c(skewness(R.D.Spend),skewness(Administration),skewness(Marketing.Spend),skewness(Profit)
  ,skewness(`dummy(State)California`),skewness(`dummy(State)Florida`),skewness(`dummy(State)New York`))
  
0.15904052 -0.47423007 -0.04506632  0.02258638  0.67552053  0.77174363  0.67552053


#####4th BM

c(kurtosis(R.D.Spend),kurtosis(Administration),kurtosis(Marketing.Spend),kurtosis(Profit)
,kurtosis(`dummy(State)California`),kurtosis(`dummy(State)Florida`),kurtosis(`dummy(State)New York`))

2.194932 3.085538 2.275967 2.824704 1.456328 1.595588 1.456328


###### Graph Tech



boxplot(R.D.Spend,Administration,Marketing.Spend,Profit,`dummy(State)California`
        ,`dummy(State)Florida`,`dummy(State)New York`)
barplot(R.D.Spend)
barplot(Administration)
barplot(Marketing.Spend)
barplot(Profit)
barplot(`dummy(State)California`)
barplot(`dummy(State)Florida`)
barplot(`dummy(State)New York`)


hist(R.D.Spend)
hist(Administration)
hist(Marketing.Spend)
hist(Profit)
hist(`dummy(State)California`)
hist(`dummy(State)Florida`)
hist(`dummy(State)New York`)

###For checking   NA  value 
is.na(R.D.Spend)
is.na(Administration)
is.na(Marketing.Spend)
is.na(Profit)
is.na(`dummy(State)California`)
is.na(`dummy(State)Florida`)
is.na(`dummy(State)New York`)


### so we dont have any Na Value 
summary(company)


str(company)
#####correlation

pairs(company)
cor(company)


###So we find that there is Partial co-relation so we 
library(corpcor)
A<- cor2pcor(cor(company))
A
summary(A)
pairs(A)

library(GGally)
ggpairs(company)
class(A)
B <- as.data.frame(A)
class(B)
ggpairs(B)
## it decrease the dependecy on R.D. spend tp MArketing.Spend 
###Now we say that there is no Partial Corelation
#### so we apply glm function  on data


model1<-lm(Profit~.,data = company) 
summary(model1)
######p value of Administration(.608), Marketing.spend(.123) ,state clifornia(.990)
###,state florida(.943) is high so..


###so we check these sepratelly

AD <- lm(Profit~Administration)
summary(AD)
###  p =.16222 less than compare to Whole data 

MS <- lm(Profit~Marketing.Spend)
summary(MS)
##p= is less than .05

SC <- lm(Profit~`dummy(State)California`)
summary(SC)

###p=.312

SF <- lm(Profit~`dummy(State)Florida`)
summary(SF)
###p=.421

SN <- lm(Profit~`dummy(State)New York`)
summary(SN)
#####p=.829


####install.packages("tidyverse")


###rename the clm
names(`dummy(State)California`) <- c("california")
names(`dummy(State)Florida`) <- c("florida")
names(`dummy(State)New York`) <- c("newyork")
View(company)
company["California"] <- names('dummy(State)California')
names(company) <- c("a","b","c","d","e","f","g")
View(company)


#p value is high so we aplly transformation

model2<- lm(Profit~(R.D.Spend)^2+(Administration)^2+(Marketing.Spend)^2+(`dummy(State)California`^2)+`dummy(State)Florida`^2+`dummy(State)New York`^2,data = company)
summary(model2)


##########it no reduce the p, value so we again applyb the transformation

model3<- lm(Profit~(R.D.Spend)^3+(Administration)^3+(Marketing.Spend)^3+factor(`dummy(State)California`^3)+factor(`dummy(State)Florida`^3)+factor(`dummy(State)New York`^3),data = company)
summary(model3)


###again tranformation

#model4<- lm(exp(Profit)~exp(R.D.Spend)+exp(Administration)+exp(Marketing.Spend)
#+exp(`dummy(State)California`)+exp(`dummy(State)Florida`)+exp(`dummy(State)New York`)
#,data = company,na.omit(Profit),na.omit(R.D.Spend),na.omit(Administration),
#na.omit(Marketing.Spend),na.omit(`dummy(State)California`)
#,na.omit(`dummy(State)Florida`),na.omit(`dummy(State)New York`))
#summary(model4)

#model5 <- lm(log(Profit)~log(`dummy(State)California`))
#summary(model5)
###########checking NA
#all(is.na(Administration))
#all(is.na(Marketing.Spend))
#all(is.na(`dummy(State)California`))
#all(is.na(`dummy(State)Florida`))
#all(is.na(`dummy(State)New York`))

#is.na(company$)
#ABC$State <- as.factor(ABC$State)
#str(ABC)
#attach(ABC)
#model <- lm(ABC$Profit~.,data=ABC)
#summary(model)

#model6 <- lm(log(ABC$Profit)~log(ABC$R.D.Spend)+log(ABC$Administration)+log(ABC$Marketing.Spend),data = ABC)
#summary(model6)


# we  know that there is a dummy variable so w cant apply log transformation bcz log1=0 log0=undefined 


#so here we find that after the transformation we cant reduce the p value so it mean that there is is a dependency so now we check dependency


install.packages("car")
library(car)

influencePlot(model3)
influenceIndexPlot(model3,id.n=2)
#outlier 50 and 49

influence.measures(model3)
# 3 outlier

#  now we will remove 50 th row 

model7 <- lm(company$Profit~.,data = company[-50,])
summary(model7)
attach(company)
ABC$State <- as.factor(ABC$State)
attach(ABC)
## do not find less p value  so remove 49
model8 <- lm(Profit~.,data=ABC[-c(49,50),])
summary(model8)
### it also not reduce the p value 
# now we check the vuf and av plots for evidence
library(car)
avPlots(model8,id.n=2)
vif(model8)
vif(alias(model8))
A<- alias(model3)
summary(A)
is.recursive(model3)
is.atomic(model3)
#### so after that we know that we will not find vif of dummy variable 
## so for remove any clm we need domain expert and subject expert also business objective
## here first i remove dummy variable and build model
### and i found that profit is on;y depend on R.D. spend 

model10 <- lm(Profit~R.D.Spend,data = company)
summary(model10)


plot(model10)
