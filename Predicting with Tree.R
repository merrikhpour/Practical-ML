
data("iris")
library(ggplot2)
names(iris)
table(iris$Species)

#create training and test set

inTrain<- createDataPartition(iris$Species, p=0.7, list = FALSE)
training<- iris[inTrain,]
testing<- iris[-inTrain,]

qplot(Petal.Width, Sepal.Width, data = training, colour=Species)

#predicting with tree  , method= rpart

library(caret)
modFit<- train(Species~., data=training, method="rpart")
print(modFit$finalModel)

#plot tree (dendrogram, not a good quality plot!)

plot(modFit$finalModel, main= "Classification Tree")
text(modFit$finalModel, use.n = TRUE, all=TRUE, cex=.8)

#better dendrogram can be made by rattle package
#install.packages("rattle")
#install.packages("rpart.plot")
library(rattle)
fancyRpartPlot(modFit$finalModel)

#predicting new values
prediction<- predict(modFit, newdata = testing)
confusionMatrix(prediction, testing$Species)
