
#Q2
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
library(caret)
library(Hmisc)
data("concrete")
set.seed(1000)
summary(concrete)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[inTrain, ]
testing = mixtures[-inTrain, ]

featurePlot(training[,-9], training$CompressiveStrength)

qplot(training$Superplasticizer, training$CompressiveStrength)

plot(training$CompressiveStrength)
FlyAsh3<- cut2(training$FlyAsh, g=3)
table(FlyAsh3)
plot(training$CompressiveStrength, col=FlyAsh3)

Age4 <- cut2(training$Age, g=4)
plot(training$CompressiveStrength, col=Age4)

Age2<- cut2(training$Age, g=2)
plot(training$CompressiveStrength, col=Age2)

qplot(Age4, training$CompressiveStrength, fill= Age4, geom = c("boxplot", "jitter"))
plot(training$CompressiveStrength, col=Age4)

#Q3
library(AppliedPredictiveModeling)
data("concrete")
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[inTrain, ]
testing = mixtures[-inTrain, ]

hist(training$Superplasticizer)
hist(log(training$Superplasticizer+1))

#Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data("AlzheimerDisease")
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain, ]
testing = adData[-inTrain, ]
head(training)

DataILpredictors <- training[,c("IL_11","IL_13","IL_16","IL_17E","IL_1alpha","IL_3","IL_4","IL_5","IL_6","IL_6_Receptor","IL_7","IL_8","diagnosis" )]
DataILpredictorstestset <- testing[,c("IL_11","IL_13","IL_16","IL_17E","IL_1alpha","IL_3","IL_4","IL_5","IL_6","IL_6_Receptor","IL_7","IL_8","diagnosis" )]

#PCA with caret

#how many principal components needed to explain 90 % of the variance
preprocessPCA <- preProcess(DataILpredictors[,-13], method = "pca", thresh = 0.9)
preprocessPCA



#Q5
#model with no PCA
modelFit<- train(diagnosis~., data = DataILpredictors, method="glm")
prediction<- predict(modelFit, newdata = testing)
confusionMatrix(testing$diagnosis, prediction)


#model with PCA

modelFit2<- train(diagnosis~., data = DataILpredictors, method="glm", preProcess="pca", trControl = trainControl(preProcOptions = list(thresh = 0.8)))
prediction2<- predict(modelFit2, testing)
confusionMatrix(prediction2, testing$diagnosis)





