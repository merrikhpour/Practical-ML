install.packages("ISLR")
library(ISLR)
library(ggplot2)
library(caret)
data("Wage")
summary(Wage)

#getting training/test set

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

#Feature plot (caret package)

featurePlot(x=training[,c("age", "education", "jobclass")], y=training$wage, plot = "pairs")

#Qplot (ggplot2 package)

qplot(age,wage, data = training) # another way: qplot(training$age, training$wage)

#Qplot with colour (ggplot2 package)

qplot(age, wage, colour=jobclass, data=training)

#add regression smoothers (ggplot2 package)

qq<- qplot(age, wage, colour=education, data=training)
qq + geom_smooth(method='lm', formula=y~x)

#cut2, making factors (Hmisc package)
install.packages("Hmisc")
library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)   

#boxplot with cut2
p1<- qplot(cutWage, training$age, fill=cutWage, geom = c("boxplot"))
p1

#boxplots with points overlayed
p2<- qplot(cutWage,training$age, fill=cutWage, geom = c("boxplot", "jitter"))
p2

#Tables
t1<- table(cutWage, training$jobclass)
t1
#proportion in each row
prop.table(t1,1)

#proportion in each col
prop.table(t1,2)


#Density plot

qplot(wage, colour=education, data=training, geom="density")