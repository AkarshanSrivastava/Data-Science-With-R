


# First data is imported and then we view the data and attach it
data<- read.csv("/Users/sandeepgautam/Desktop/Assignments/Multi Linear Regression/ToyotaCorolla.csv")

View(data)

names(data)


# First we will remove all the columns which aren't going to be used while building regression model as per the question.

newdata <- data[,c(3,4,7,9,13,14,16,17,18)]

# Analysing Correlation between variables 
pairs.panels(newdata)

library(corpcor)
aa <- cor2pcor(cor(newdata))

library(corrplot)
corrplot(aa, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

attach(newdata)
summary(newdata)
boxplot(newdata)


# From box plot we see that variable 'KM' doesn't keep to follow normality .So we will further check it.
plot(Price,KM)
plot(Price,log(KM))

# First let us build the model 

model_21 <-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(model_21)

# cc and doors have more than 0.05 p value .so let's use log in KM
model_22 <-lm(Price~Age_08_04+log(KM)+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(model_22)

# it didn't work out either ,how let us see for multicolinearity
vif(model_21)

# Values <10 so no multi colinearity .Now let us  see the influence plot and see if there are some outliers observation that we can remove

influence.measures(model_21)
influenceIndexPlot(model_21,id.n=3) # index plots for infuence measures
influencePlot(model_21,id.n=3) 

# Now let's remove major  outliers and build the model

model_23 <-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=data[-81,-222,-961])
summary(model_23)

# We still have doors p value greater than 0.05. Now we will analyse added variable plot to see the effect of each variable in the residual/model.

avPlots(model_23,id.n=2,id.cex=0.7)

# Doors variable doesn't seem to have any relationship with the model. We wil further analyse it
model_24 <- lm(Price~Doors)
summary(model_24)

# R squared is only 0.03 so we see remove the Door variable and see if the model works
model_25 <-lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data=data[-81,-222,-961])
summary(model_25)

# Finally the model seems working as all the p value is less than 0.05 and R squared value is 0.86.

# There may be some error in the model please give suggestion and please explain what Avplots does actually I understand it somewhat but I am not still cleareven after researching it in the internet