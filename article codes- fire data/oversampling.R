library(unbalanced)
data=read.table(file.choose(),header=T)
MM1=as.factor(M1)
MM2=as.factor(M2)
MM3=as.factor(M3)
MM6=as.factor(M6)
MM7=as.factor(M7)
MM8=as.factor(M8)
str(data)
#data1=cbind(MM1,MM2,MM3,M4,M5,MM6,MM7,MM8)
#data1
#str(data1)
n <- ncol(data)
output <- data[ ,n]
input <- data[ ,-n]
set.seed(1234)
#apply oversampling
data2 <- ubBalance(X=input, Y=MM8, type="ubOver", k=0)
#oversampled dataset
overData <- data.frame(X, Class=Y)
#check the frequency of the target variable after oversampling
summary(overData$Class)
