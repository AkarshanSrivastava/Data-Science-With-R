calories <- read.csv(file.choose())
plot(calories)
pairs(calories)
cor(calories$Calories.Consumed,calories$Weight.gained..grams.)


model1 <- lm(calories$Weight.gained..grams.~calories$Calories.Consumed)
summary(model1)
predict1<- predict(model1)
error <- predict1-calories$Weight.gained..grams.
error
rms <- (sqrt(sum(error)^2))/(nrow(calories))


###trnas

model2 <- lm(log(calories$Calories.Consumed)~log(calories$Weight.gained..grams.))
summary(model2)
cor(log(calories$Weight.gained..grams.),log(calories$Calories.Consumed))
predict2 <- exp(predict(model2))
error2 <- predict2-calories$Weight.gained..grams.
rms2 <- (sqrt(sum(error2)^2))/(nrow(calories))
         