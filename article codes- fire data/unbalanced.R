data=read.table(file.choose(),header=T)
str(data)
attach(data)
M1F=as.factor(M1)
M2F=as.factor(M2)
M3F=as.factor(M3)
M6F=as.factor(M6)
M7F=as.factor(M7)
M8F=as.factor(M8)
Dat=data
#install.packages("ROSE")
library(ROSE)
str(data)
table(M8F)
prop.table(table(M8F))
#install.packages("rpart")
library(rpart)
tree.fire=rpart(M8F~M1F+M2F+M3F+M4+M5+M6F+M7F)
pred.tree.fire=predict(tree.fire)
plot(pred.tree.fire)
accuracy.meas(M8F, pred.tree.fire[,1])

Datx=data.frame(M1F,M2F,M3F,M4,M5,M6F,M7F)
#install.packages("unbalanced")
library(unbalanced)
n<-ncol(Dat)
output<- M8F
input<- Datx
View(input)

data2=ubBalance(X= input, Y=output, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
View(data2)
str(data2)
attach(data2)
names(data2)
bData=cbind(X,Y)
#bData<-cbind(X.M1F,X.M2F,X.M3F,X.M4,X.M5,X.M6F,X.M7F,Y)
View(bData)
table(Y)
























