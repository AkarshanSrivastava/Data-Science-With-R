##here we find the sorting time 
### FIND COR and rms value of EVERY MODEL 
delivery <-  read.csv(file.choose())
View(delivery)
summary(delivery)

attach(delivery)
plot(Delivery.Time,Sorting.Time)
cor(Delivery.Time,Sorting.Time)
.82 moderate 

####Apply transformation
plot(log(Delivery.Time),log(Sorting.Time))
cor(log(Delivery.Time),log(Sorting.Time))
.87  Stronf Corelation


#slr


d <- lm(Sorting.Time~Delivery.Time)
summary(d)
### here p is high and R^2 .68 less so apply transformation

t<- lm(log(Sorting.Time)~log(Delivery.Time))
summary(t)
  

predict(t,interval="predict")
t$fitted.values

log(Sorting.Time)

#####1st Error =2.07-2.30=-.23
