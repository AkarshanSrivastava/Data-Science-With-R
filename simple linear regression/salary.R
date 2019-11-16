#######calculate the year of exp

salary <- read.csv(file.choose())
View(salary)
summary(salary)

attach(salary)
plot(salary,YearsExperience)
cor(salary,YearsExperience)
#####it shows
#> cor(salary,YearsExperience)
#[,1]
#YearsExperience 1.0000000
#Salary          0.9782416

#so both are strong cor
##please discuss this point 

s <- lm(YearsExperience~Salary)
summary(s)
s$fitted.values
#error=1.6-1.1=.5



### FIND COR and rms value of EVERY MODEL 