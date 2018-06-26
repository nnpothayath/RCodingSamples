#Question3

#a

set.seed(1)
x <- rnorm(100)
e <- rnorm(100)

#b

b0 <- 2
b1 <- 3
b2 <- -1
b3 <- -3
y <- b0 + b1 * x + b2 * x^2 + b3 * x^3 + e

#c


install.packages("leaps")
library(leaps)
data <- data.frame(y = y, x = x)
reg <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + 
              I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data)
regsummary <- summary(reg)
par(mfrow = c(2, 2))
plot(regsummary$cp, xlab = "No. of variables", ylab = "Cp", type = "l")
points(which.min(regsummary$cp), regsummary$cp[which.min(regsummary$cp)])
plot(regsummary$bic, xlab = "No. of variables", ylab = "BIC", type = "l")
points(which.min(regsummary$bic), regsummary$bic[which.min(regsummary$bic)])
plot(regsummary$adjr2, xlab = "No. of variables", ylab = "R^2 Adj.", type = "l")
points(which.max(regsummary$adjr2), regsummary$adjr2[which.max(regsummary$adjr2)])

coef(reg, which.max(regsummary$adjr2))


#forward stepwise selection

regforward <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) +
                           I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data, method = "forward")
regforwardsummary <- summary(regforward)
par(mfrow = c(2, 2))
plot(regforwardsummary$cp, xlab = "No of variables", ylab = "Cp", type = "l")
points(which.min(regforwardsummary$cp), regforwardsummary$cp[which.min(regforwardsummary$cp)])
plot(regforwardsummary$bic, xlab = "No of variables", ylab = "BIC", type = "l")
points(which.min(regforwardsummary$bic), regforwardsummary$bic[which.min(regforwardsummary$bic)])
plot(regforwardsummary$adjr2, xlab = "No of variables", ylab = "R^2 Adj.", type = "l")
points(which.max(regforwardsummary$adjr2), regforwardsummary$adjr2[which.max(regforwardsummary$adjr2)])

coef(regforward, which.max(regforwardsummary$adjr2))

#backward stepwise selection

regbackward <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10),
                          data = data, method = "backward")
regsummarybackward <- summary(regbackward)
par(mfrow = c(2, 2))
plot(regsummarybackward$cp, xlab = "No. of variables", ylab = "Cp", type = "l")
points(which.min(regsummarybackward$cp), regsummarybackward$cp[which.min(regsummarybackward$cp)])
plot(regsummarybackward$bic, xlab = "No. of variables", ylab = "BIC", type = "l")
points(which.min(regsummarybackward$bic), regsummarybackward$bic[which.min(regsummarybackward$bic)])
plot(regsummarybackward$adjr2, xlab = "No. of variables", ylab = "R^2 Adj.", type = "l")
points(which.max(regsummarybackward$adjr2), regsummarybackward$adjr2[which.max(regsummarybackward$adjr2)])

coef(regbackward, which.max(regsummarybackward$adjr2))

#e

install.packages("glmnet")
library(glmnet)

matrix <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + 
                         I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data)[, -1]
cv.lasso <- cv.glmnet(matrix, y, alpha = 1)
plot(cv.lasso)

minimumlambda <- cv.lasso$lambda.min
print(minimumlambda)

fit.lasso <- glmnet(matrix, y, alpha = 1)
predict(fit.lasso, s = minimumlambda, type = "coefficients")[1:11, ]

#f

b7 <- 7
y <- b0 + b7 * x^7 + e
data <- data.frame(y = y, x = x)
regfitfull <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + 
                            I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data)
regsummary <- summary(regfitfull)
par(mfrow = c(2, 2))
plot(regsummary$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
points(which.min(regsummary$cp), regsummary$cp[which.min(regsummary$cp)])
plot(regsummary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(regsummary$bic), regsummary$bic[which.min(regsummary$bic)])
plot(regsummary$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(regsummary$adjr2), regsummary$adjr2[which.max(regsummary$adjr2)])

coef(regfitfull, 2)
coef(regfitfull, 1)
coef(regfitfull, 4)

matrix <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + 
                       I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data)[, -1]
cv.lasso <- cv.glmnet(matrix, y, alpha = 1)
minimumlambda <- cv.lasso$lambda.min
print(minimumlambda)

fit.lasso <- glmnet(matrix, y, alpha = 1)
predict(fit.lasso, s = minimumlambda, type = "coefficients")[1:11, ]

#Question 4

#a
library(ISLR)
set.seed(11)
sum(is.na(College))

trainsize <- dim(College)[1] / 2
train<-sample(1:dim(College)[1], trainsize)
test <- -train
Collegetrainset <- College[train, ]
Collegetestset <- College[test, ]

#b

lm <- lm(Apps~., data=Collegetrainset)
lmpredicted <- predict(lm, Collegetestset)
mean((Collegetestset[, "Apps"] - lmpredicted)^2)

#c

library(glmnet)
trainmatrix <- model.matrix(Apps~., data=Collegetrainset)
testmatrix <- model.matrix(Apps~., data=Collegetestset)
grid <- 10 ^ seq(4, -2, length=100)
modridge <- cv.glmnet(trainmatrix, Collegetrainset[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
lambdaminimum <- modridge$lambda.min
print(lambdaminimum)

ridgepred <- predict(modridge, newx=testmatrix, s=lambdaminimum)
mean((Collegetestset[, "Apps"] - ridgepred)^2)

#d
modusinglasso <- cv.glmnet(trainmatrix, Collegetrainset[, "Apps"], alpha=1, lambda=grid, thresh=1e-12)
lambdaminimum <- modusinglasso$lambda.min
print(lambdaminimum)

lassoprediction <- predict(modusinglasso, newx=testmatrix, s=lambdaminimum)
mean((Collegetestset[, "Apps"] - lassoprediction)^2)

modusinglasso <- glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=1)
predict(modusinglasso, s=lambdaminimum, type="coefficients")

#e
install.packages("pls")
library(pls)

pcr <- pcr(Apps~., data=Collegetrainset, scale=T, validation="CV")
validationplot(pcr, val.type="MSEP")

pcrpredicted <- predict(pcr, Collegetestset, ncomp=10)
mean((Collegetestset[, "Apps"] - data.frame(pcrpredicted))^2)

#f

pls <- plsr(Apps~., data=Collegetrainset, scale=T, validation="CV")
validationplot(pls, val.type="MSEP")
plspredicted <- predict(pls, Collegetestset, ncomp=10)
mean((Collegetestset[, "Apps"] - data.frame(plspredicted))^2)

#g

testaverage <- mean(Collegetestset[, "Apps"])
lmrsq <- 1 - mean((Collegetestset[, "Apps"] - lmpredicted)^2) /mean((Collegetestset[, "Apps"] - testaverage)^2)
lassorsq <- 1 - mean((Collegetestset[, "Apps"] - lassoprediction)^2) /mean((Collegetestset[, "Apps"] - testaverage)^2)
pcrrsq <- 1 - mean((Collegetestset[, "Apps"] - pcrpredicted)^2) /mean((Collegetestset[, "Apps"] - testaverage)^2)
plsrsq <- 1 - mean((Collegetestset[, "Apps"] - plspredicted)^2) /mean((Collegetestset[, "Apps"] - testaverage)^2)
ridgersq <- 1 - mean((Collegetestset[, "Apps"] - ridgepred)^2) /mean((Collegetestset[, "Apps"] - testaverage)^2)
print(lmrsq)
print(lassorsq)
print(pcrrsq)
print(plsrsq)
print(ridgersq)
