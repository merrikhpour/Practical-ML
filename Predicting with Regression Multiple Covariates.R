
#Wage data

library(caret)
library(ISLR)
library(ggplot2)
data("Wage")

#subseting data for the exploratory purposes- exckuding the variable we are trying to predict

Wage<-subset(Wage, select = -c(logwage))
summary(Wage)

#getting traing and testing sets

inTrain<- createDataPartition(y=Wage$wage, p=0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

featurePlot(x=training[,c("age", "education", "jobclass")], y= training$wage, plot="pairs")
qplot(age, wage, data = training)
qplot(age,wage, colour=jobclass, data=training)

#Fit a linear model

modFit <- train(wage ~ age+education+jobclass, method="lm", data=training)
finMod <- modfit$finalModel
print(modFit)

#Diagnostics

plot(finMod, 1, pch=19, cex=0.5, col="#00000010")

#color by varialbes not used in the model

qplot(finMod$fitted, finMod$residuals, colour=race, data=training)

plot(finMod$residuals, pch=19)

#predicted vs truth in test set

pred<- predict(modFit, testing)
qplot(wage, pred, colour=year, data=testing)
