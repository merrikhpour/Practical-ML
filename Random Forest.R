data("iris")
library(ggplot2)
library(caret)

inTrain <- createDataPartition(iris$Species, p=0.7, list = F)
training <- iris[inTrain,]
testing<- iris[-inTrain,]


#random Forest , rf

modelfit <- train(Species~., data = training, method="rf", prox=TRUE)
modelfit

#getting a single tree
getTree(modelfit$finalModel, k=2)

#predicting new values
pred<- predict(modelfit, newdata = testing)
testing$predright <- pred==testing$Species
table(pred, testing$Species)

qplot(testing$Petal.Width, testing$Petal.Length, colour=predright, data=testing)


