#ozone data
#install.packages("ElemStatLearn")
library(ElemStatLearn)
data("ozone")

ozone<- ozone[order(ozone$ozone),]
head(ozone)
str(ozone) 

#bagged loess
ll<- matrix(NA, nrow = 10, ncol = 111)
for (i in 1:10){ #resample dataset for 10 different times
        ss<-sample(1:dim(ozone)[1], replace = T) #resample with replacement from the entire dataset
        ozone0<- ozone[ss,]
        ozone0<- ozone0[order(ozone0$ozone),]
        loess0<- loess(temperature~ozone, data=ozone0, span = 0.2) #fit loess curve (its a smooth curve) , span is a messure of how smooth the curve to be
        ll[i,] <- predict(loess0, newdata=data.frame(ozone=1:111))
}

#plot c

plot(ozone$ozone, ozone$temperature, pch=19, cex=0.5)
for (i in 1:10) {lines(1:111, ll[i,], col="grey", lwd=2)}
lines(1:111, apply(ll,2,mean), col="red", lwd=2)

#bagging in caret: in train function consider method options : bagEarth , treebag, bagFDA