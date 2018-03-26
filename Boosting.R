library(ISLR)
data("Wage")
library(ggplot2)
library(caret)

Wage<- subset(Wage, select=-c(logwage))
inTrain<- createDataPartition(Wage$wage, p=0.7, list=F)
training<- Wage[inTrain,]
testing<-Wage[-inTrain,]

modFit<- train(wage~. , method="gbm", data = training, verbose=F)
print(modFit)