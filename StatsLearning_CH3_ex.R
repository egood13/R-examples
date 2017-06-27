
# hw problems in Elements of Statistical Learning

#### Chapter 3 Problems ####
setwd('./R/Kaggle/statslearning')

#8

Auto <- read.csv('Auto.csv')
Auto <- na.omit(Auto)
Auto$horsepower <- as.integer(Auto$horsepower)
summary(Auto)

lm.fit <- lm(mpg~horsepower, data = Auto)
summary(lm.fit)
# the p-value indicates there is a relation between the two variables.
# the relation is a weak, positive relation as beta_1 = 0.11080
predict(lm.fit, data.frame(horsepower=98), interval = "confidence")
predict(lm.fit, data.frame(horsepower=98), interval = "prediction")

plot(Auto$mpg, Auto$horsepower)
abline(lm.fit)
par(mfrow=c(1,1))
plot(lm.fit)
# the residuals vs fitted shows that there's a jump in the tendency of the
# variables as the mpg as a response of horsepower jumps from 22 to 24
