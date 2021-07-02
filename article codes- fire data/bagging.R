data=read.table(file.choose(),header=T)
str(data)
attach(data)
M1F=as.factor(M1)
M2F=as.factor(M2)
M3F=as.factor(M3)
M6F=as.factor(M6)
M7F=as.factor(M7)
M8F=as.factor(M8)

library(randomForest)
set.seed (1)
bag.boston =randomForest(M8F~M1F+M2F+M3F+M4+M5+M6F+M7F  ,
mtry=5, importance =TRUE)
bag.boston