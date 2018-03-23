#creating trainign and test set
library(ISLR)
library(caret)
data("Wage")

inTrain <- createDataPartition(Wage$wage, p=0.7, list = FALSE)
training <- Wage[inTrain,]
testing<- Wage[-inTrain,]

#turning qualitative values to quantitative (for example defining dummy variables)

table(training$jobclass)
dummies<- dummyVars(wage~jobclass, data = training)
head(predict(dummies, newdata = training))

#removing zero covariates
nzv<- nearZeroVar(training, saveMetrics = TRUE)  #if its true it means the variance is close to zero and 
#it wont be a good predictor
nzv 

# basis function
# is used to fit curvy line to the model in oppose to straight line 
# which are use in linear regression or generalized linear regression

library(splines)
bsBasis <- bs(training$age, df= 3)  #bs function creates polynomial variable
#df=3 used for 3rd degree polynomial
bsBasis

#fitting curves with splines

lm1 <- lm(wage~ bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1,newdata = training), col="red", pch=19, cex=0.5)

#splines on the test set  (you have to predict those SAME variables)
#covariates on the test data set are created using the exact same procedure used 
#for training set
#
predict(bsBasis, age=testing$age)  #as opposed to applying bs function on testist set
