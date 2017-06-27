
#check the directory is set to Kaggle

###PROB 8###
#a)
college <- read.csv("college.csv")
fix(college)
#b)

rownames(college) <- college[,1]
fix(college)

#c)

summary(college)

pairs(college[,1:10])

boxplot(Outstate ~ Private, college)

Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college)
boxplot(Outstate ~ Private, college)

hist(college[,6])


####Problem 9####

Auto <- read.table("Auto.data", header=TRUE)
Auto = na.omit(Auto)
head(Auto)
#a)
unique(Auto$origin)
# name and origin are qualitative variables
#b)
sapply(Auto[,1:7], is.numeric)
Auto$horsepower <- as.numeric(Auto$horsepower)
sapply(Auto[,1:7], range)
#c)
sapply(Auto[,1:7], mean)
sapply(Auto[,1:7], sd)
#d)
sapply(Auto[-(10:85),1:7],range)
sapply(Auto[-(10:85),1:7],mean)
sapply(Auto[-(10:85),1:7],sd)
#e)
pairs(Auto)
# Positive correlation between displacement and weight; negative correlation
# between mpg and displacement, mpg and wieight, displacement and acceleration



##### PROBLEM 10 ####

#a)

library(MASS)
?Boston

#b)

pairs(Boston)
# Posotive correlation between crim and nox, crim and age; positive correlation
# between medv and rm; negative correlation between medv and lstat, dls and nox
#c)
par(mfrow=c(2,2))
plot(Boston$crim, Boston$nox)
plot(Boston$crim, Boston$age)
plot(Boston$crim, Boston$medv)
plot(Boston$crim, Boston$dls)

plot(Boston$crim, Boston$age)
plot(Boston$crim, Boston$ptratio)
plot(Boston$crim, Boston$tax)
plot(Boston$crim, Boston$lstat)

# As medv increase, crim decreases; small correlation between crim and lstat.
# Also, positive correlation between crim and age, ptratio, tax.

#d)

plot(Boston$crim)
plot(Boston$tax)
plot(Boston$ptratio)
sapply(Boston[,1:14], range)
# suburbs with index 350:450 have most of the crime rate, high property tax, and 
# ptratio - urban areas that have mostly non-residential buildings or apartments
# (due to high tax rate)

#e)
sum(Boston$chas)

#f)
median(Boston$ptratio)

#g)
min(Boston$medv)
#index 5
Boston[5,]
sapply(Boston[,1:14], range)
# low crime rate, median age, houses with high number of rooms, lower tax, max
# value of black, low lstat

#h)

length(Boston$rm[Boston$rm > 7])
length(Boston$rm[Boston$rm > 8])

Boston[Boston$rm > 8,]
# low crim, high black/ptratio
