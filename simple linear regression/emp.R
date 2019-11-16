######calculate salary hike 
### FIND COR and rms value of EVERY MODEL 
employee <- read.csv(file.choose())
View(employee)
summary(employee)

attach(employee)
cor(Churn_out_rate,Salary_hike)
-..911  ###negative stronf corelation

c <- lm(Salary_hike~Churn_out_rate)
summary(c)

#########Bo is to high 
predict(c,interval="predict")
c$fitted.values



####transformation

cor(log(Churn_out_rate),log(Salary_hike))
-.94#negative strong
plot(log(Churn_out_rate),log(Salary_hike))


p <- lm(log(Salary_hike)~log(Churn_out_rate),)
summary(p)

Bo=9.00
B1=-.367

p$fitted.values
log(Salary_hike)


#######to much less error 