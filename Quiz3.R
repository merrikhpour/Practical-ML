library(AppliedPredictiveModeling)
data("segmentationOriginal")
library(caret)

training=segmentationOriginal[segmentationOriginal$Case=="Train",]
dim(training)

testing=segmentationOriginal[segmentationOriginal$Case=="Test",]
dim(testing)

set.seed(125)

fitmodel= train(Class~., method="rpart", data=training)
fitmodel$finalModel
prediction <- predict(fitmodel, newdata = testing)

library(rattle)
fancyRpartPlot(fitmodel$finalModel)



#######Q3

load("olive")
summary(olive)
head(olive)
dim(olive)

newdata= as.data.frame(t(colMeans(olive)))
fitmodel= train(Area~., method="rpart", data=olive)
fitmodel$finalModel

fancyRpartPlot(fitmodel$finalModel)
prediction<- predict(fitmodel, newdata = newdata)
prediction

#######Q4

library(ElemStatLearn)
data("SAheart")
set.seed(8484)
train=sample(1:dim(SAheart)[1], size = dim(SAheart)[1]/2, replace = F)
trainSA=SAheart[train,]
testSA=SAheart[-train,]

set.seed(13234)
modelfit= train(chd~age+alcohol+obesity+typea+tobacco+ldl, method="glm", family="binomial", data=trainSA)
modelfit$finalModel
values<-trainSA$chd
prediction<- predict(modelfit, newdata = trainSA)
missClass <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(values, prediction)

values2<- testSA$chd
prediction2<- predict(modelfit, newdata = testSA)
missClass(values2, prediction2)


#####Q6

library(ElemStatLearn)
data("vowel.test")
data("vowel.train")
head(vowel.test)
head(vowel.train)

vowel.train$y<- as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)
set.seed(33833)

modelfit<- train(y~., method="rf", data=vowel.train)
modelfit2<- randomForest(y~., data = vowel.train)
varImp(modelfit2)
order(varImp(modelfit2), decreasing = T)




