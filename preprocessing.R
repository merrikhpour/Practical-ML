#install.packages("caret", dependencies = TRUE)
library(caret)
library(kernlab)
data("spam")

inTrain<- createDataPartition(y=spam$type, p=0.7, list = FALSE)
training<- spam[inTrain, ]
testing<- spam[-inTrain, ]
hist(training$capitalAve, main = "", xlab="ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)

#sd is much higher than mean which shows the data is highly variable 
#one way to fix this is standarzization

trainCapAve<-training$capitalAve
trainCapAveS<- (training$capitalAve-mean(training$capitalAve))/sd(training$capitalAve)
mean(trainCapAveS)
sd(trainCapAveS)

#preprocess function (caret package) to do standardization
preObj<- preProcess(training[,-58], method = c("center", "scale"))
trainCapAveS2 <- predict(preObj,training[,-58])$capitalAve  
mean(trainCapAveS2)
sd(trainCapAveS2)

#now we can apply same values calculated in preObj to standardize test set
testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)   #note that mean will be CLOSE to 0 and sd CLOSE to 1 since we used 
# parameters of training set (mean and sd of training set calculated by preObj)

set.seed(32343)
modelFit2<- train(type ~. , data= training, preProcess=c("center", "scale"), method="glm")
modelFit2

#Box-Cox transformation

preObj<- preProcess(training[,-58], method = c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)


#imputing data with K nearest neighbour method

set.seed(13343)
#make some value NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05)==1
training$capAve[selectNA]<- NA

#impute and standardize
preObj<- preProcess(training[,-58], method = c("knnImpute"))
capAve<- predict(preObj, training[,-58])$capAve

#Standardize true values
capAveTruth <- training$capitalAve
capAveTruth<- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

#to see how close the values we imputed and actual values we can look at the 
#quntiles of their difference
quantile(capAve-capAveTruth)

#only for the ones that were missinh
quantile((capAve-capAveTruth)[selectNA])
#for those not being NA
quantile((capAve-capAveTruth)[!selectNA])


