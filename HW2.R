#Question 4

#a

install.packages("ISLR")
library(ISLR)
summary(Weekly)

#Since Direction is qualitative, we are not taking that column
cor(Weekly[,-9])
attach(Weekly)
plot(Volume~Year)

#b

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, 
          family = binomial)
summary(glm.fit)

#c

probability = predict(glm.fit, type = "response")
predictor = rep("Down", length(probability))
predictor[probability > 0.5] <- "Up"
table(predictor, Direction)

#d

training = (Year < 2009)
Weeklyof2009to2010 = Weekly[!training, ]
glm.fit1 = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = training)
prob = predict(glm.fit1, Weeklyof2009to2010, type = "response")
prediction = rep("Down", length(prob))
prediction[prob > 0.5] = "Up"
Direction1 = Direction[!training]
table(prediction, Direction1)

#e
install.packages("MASS")
library(MASS)
lda = lda(Direction ~ Lag2, data = Weekly, subset = training)
print(lda)

predictedLda <- predict(lda, Weeklyof2009to2010)
table(predictedLda$class, Direction1)

#f

qda = qda(Direction ~ Lag2, data = Weekly, subset = training)
print(qda)
predictedqda = predict(qda, Weeklyof2009to2010)
table(predictedqda$class, Direction1)

#g

library(class)
train1 = matrix(Lag2[training])
test1 = matrix(Lag2[!training])
Direction2 <- Direction[training]
set.seed(1)
predictedknn <- knn(train1, test1, Direction2, k = 1)
table(predictedknn, Direction1)

#i

fitforL1L2 = glm(Direction ~ Lag2:Lag1, data = Weekly, family = binomial, subset = training)
prob = predict(fitforL1L2, Weeklyof2009to2010, type = "response")
predictL1L2 = rep("Down", length(prob))
predictL1L2[prob > 0.5] = "Up"
Direction1 = Direction[!training]
table(predictL1L2, Direction1)

lda = lda(Direction ~ Lag2:Lag1, data = Weekly, subset = training)
predictedLda <- predict(lda, Weeklyof2009to2010)
table(predictedLda$class, Direction1)
mean(predictedLda$class == Direction1)

qda = qda(Direction ~ Lag2:Lag1, data = Weekly, subset = training)
predictedqda = predict(qda, Weeklyof2009to2010)
table(predictedqda$class, Direction1)

predictedknn = knn(train1, test1, Direction2, k = 10)
table(predictedknn, Direction1)

predictedknn = knn(train1, test1, Direction2, k = 100)
table(predictedknn, Direction1)

#Question 5

#a

library(ISLR)
summary(Auto)
attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(Auto, mpg01)

#b

cor(Auto[, -9])
pairs(Auto)
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01 variable")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01 variable")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01 variable")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01 variable")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01 variable")
boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01 variable")

#c

train = (year > 75)  
test = !train
Autotrain = Auto[train, ]
Autotest = Auto[test, ]
mpg01test = mpg01[test]

#d

library(MASS)
lda12 = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
ldapred = predict(lda12, Autotest)
mean(ldapred$class != mpg01test)

#Error Rate of 16.67%

#e

qdafit = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
qdapred = predict(qdafit, Autotest)
mean(qdapred$class != mpg01test)

#Error Rate of 10.55%

#f. 

lr = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto,
              family = binomial, subset = train)
lrprobs = predict(lr, Autotest, type = "response")
lrpred = rep(0, length(lrprobs))
lrpred[lrprobs > 0.5] = 1
mean(lrpred != mpg01test)

#Error Rate of 16.11%

#g.

library(class)
train1 = cbind(cylinders, weight, displacement, horsepower)[train, ]
test1 = cbind(cylinders, weight, displacement, horsepower)[test, ]
train1.mpg01 = mpg01[train]
set.seed(1)
# KNN(k=1)
knnpred = knn(train1, test1, train1.mpg01, k = 1)
mean(knnpred != mpg01test)

#Error rate of 16.67% with K=1

knnpred = knn(train1, test1, train1.mpg01, k = 10)
mean(knnpred != mpg01test)

#Error Rate of 16.11% with K=10

knnpred = knn(train1, test1, train1.mpg01, k = 100)
mean(knnpred != mpg01test)

#Error Rate of 19.44% with K = 100

#Question 7

#g
pr = function(n) 
{
  return(1 - (1 - 1/n)^n)
  
}
x = 1:100000
plot(x, pr(x))

#h

store=rep(NA, 10000)
for(i in 1:10000) {
  store[i]=sum(sample (1:100, rep=TRUE)==4) >0
}
mean(store)

#Question 8

library(ISLR)
summary(Default)
attach(Default)
set.seed(1)

#a
glmQ8 = glm(default ~ income + balance, data = Default, family = binomial)

#b

#i
train = sample(dim(Default)[1], dim(Default)[1]/2)

#ii
glmQ81 = glm(default ~ income + balance, data = Default, family = binomial, subset = train)

#iii
glmpredQ8 = rep("No", dim(Default)[1]/2)
glmprobQ8 = predict(glmQ81, Default[-train, ], type = "response")
glmpredQ8[glmprobQ8 > 0.5] = "Yes"

#iv
output = mean(glmpredQ8 != Default[-train, ]$default)
print(output)

#c Repeated above thrice

#d


train = sample(dim(Default)[1], dim(Default)[1]/2)

glmQ81 = glm(default ~ income + balance + student, data = Default, family = binomial, subset = train)

glmpredQ8 = rep("No", dim(Default)[1]/2)
glmprobQ8 = predict(glmQ81, Default[-train, ], type = "response")
glmpredQ8[glmprobQ8 > 0.5] = "Yes"
output = mean(glmpredQ8 != Default[-train, ]$default)
print(output)

#Question 9

library(ISLR)
summary(Default)
attach(Default)

#a

set.seed(1)
glmfit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glmfit)

#b

boot.fn = function(data, index) 
{ 
  return(coef(glm(default ~ income + balance, data = data, family = binomial, subset = index)))
}
 
#c

library(boot)
boot(Default, boot.fn, 30)
