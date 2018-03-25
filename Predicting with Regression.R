
library(caret)
data("faithful")
set.seed(333)

inTrain<- createDataPartition(faithful$waiting, p=0.5, list = FALSE)
trainFaith <- faithful[inTrain,]
testFaith<- faithful[-inTrain,]
head(trainFaith)

plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")

lm1<- lm(eruptions ~waiting, data=trainFaith)
summary(lm1)

plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")
lines(trainFaith$waiting, lm1$fitted.values, lwd=3)  

#plot predictions- training and testing

par(mfrow=c(1,2))
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")
lines(trainFaith$waiting, predict(lm1), lwd=3)

plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")
lines(testFaith$waiting, predict(lm1, newdata=testFaith), lwd=3)

#Get training set/test set error
#calcuate RMSE (root mean square error) on training
sqrt(sum((trainFaith$eruptions-lm1$fitted.values)^2))

#calculate RMSE for test set
sqrt(sum((predict(lm1, newdata=testFaith)-testFaith$eruptions)^2))


#prediction interval
predic1<- predict(lm1, newdata=testFaith, interval="prediction")
ord<- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue")
matlines(testFaith$waiting, predic1, type="l", col = c(1,2,2), lty = c(1,1,1), lwd = 3)



#same process with caret

modfit<- train(eruptions ~ waiting, data=trainFaith, method="lm")
summary(modfit$finalModel)