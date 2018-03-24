

#creating training and testing set

library(caret)
library(kernlab)
data("spam")

inTrain <- createDataPartition(y=spam$type, p=0.75, list = FALSE)
training<- spam[inTrain,]
testing<- spam[-inTrain,]


#taking absolute values of correlation among all covariates (58th col is "type" and exclude)

m<- abs(cor(training[,-58]))

#diagonal is the correlation of each var with its self so we set diag to 0

diag(m)<- 0

which(m>0.8, arr.ind = T) #finding highly correlated variables, results shows coloums
#34 and 32 are highly correlated


#principal component in R on subset of spam data

smallspam<- spam[,c(32,34)]
prComp <- prcomp(smallspam)
plot(prComp$x[,1], prComp$x[,2])
prComp$rotation

#PCA on spam data

typecolour <- ((spam$type=="spam")*1+1)  #spam will be red, nonespam will be black
prComp <- prcomp(log10(spam[,-58]+1))  #log10 transformation is used to make data more normal
plot(prComp$x[,1], prComp$x[,2], col=typecolour, xlab= "PC1", ylab= "PC2")

#PCA with caret

preprocessPCA <- preProcess(log10(spam[,-58]+1), method = "pca", pcaComp = 2)
 spampca<- predict(preprocessPCA, log10(spam[,-58]+1))
 plot(spampca[,1], spampca[,2], col= typecolour)
 modelFit <- train(training$type ~., method="glm", data=spampca)
 
 #test set uses same compenents we calculated for training set
 testPC<- predict(preprocessPCA, log10(testing[,-58]+1))
 confusionMatrix(testing$type, predict(modelFit,testPC))
 
 #*************************#
 #ALTERNATIVE to not use the predict function seperately
modelFit <- train(training$type ~., method="glm", preProcess="pca", data=training)
confusionMatrix(testing$type, predict(modelFit, testing))