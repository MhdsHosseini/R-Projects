#delet memory
rm(list=ls())

#read data
data1=read.csv("hadese.csv",header=F)
data2=read.csv("atash.csv",header=F)

attach(data1)
str(data1)

attach(data2)
str(data2)

#####################################################

#stat
#table(data1$V4,data1$V1)
#table(data1$V4,data1$V2)
#table(data1$V4,data1$V3)

#table(data2$V4,data2$V1)
#table(data2$V4,data2$V2)
#table(data2$V4,data2$V3)


#hadese

#sampling

set.seed(1)


r1=c(1:4359)
data1=cbind(data1,r1)

s1=data1$r1[data1$V4==0]
row1=sample(s1,500)
o1=data1$r1[data1$V4==1]
sample1=rbind(data1[row1,],data1[o1,])

str(sample1)


#data prepration

table(sample1$V4)

##partition

radif1=c(1:722)
TrainRows1=sample(radif1,dim(sample1)[1]*0.7)
TestRows1=setdiff(radif1,TrainRows1)
TrainData1=sample1[TrainRows1,]
TestData1=sample1[TestRows1,]
names(TestData1)

#modeling

library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(party)

#GLM########################################################

fit1=glm(TrainData1$V4~V1+factor(V2)+factor(V3),data=TrainData1,family=binomial())
summary(fit1)

predict1=predict(fit1, newdata =TrainData1, type = "response")


typeof(predict1)
length(predict1)
typeof(as.factor(TrainData1$V4))
length(TrainData1$V4)

pred1=ifelse(predict1 > 0.5,1,0)

typeof(pred1)
table(pred1)

CM1=confusionMatrix(data=as.factor(pred1), reference =as.factor(TrainData1$V4))
CM1

#tree###########################################
# testtype: "Bonferroni","MonteCarlo","Univariate","Teststatistic" 
tree1=ctree(factor(TrainData1$V4)~factor(V1)+factor(V2)+factor(V3),data=TrainData1, 
            controls = ctree_control(teststat = "quad",
                                     testtype = "Univariate",
                                     mincriterion = 0.95,
                                     minsplit = 1,
                                     minbucket = 3,stump = FALSE,maxdepth=3  ))
tree1
plot(tree1)
predtree1=predict(tree1)
length(predict(tree1))
CMtree1=confusionMatrix(data=as.factor(predtree1), reference =as.factor(TrainData1$V4))
CMtree1

predTest1=predict(tree1,TestData1)
CMtreet1=confusionMatrix(data=as.factor(predTest1), reference =as.factor(TestData1$V4))
CMtreet1



#atash

#sampling

r2=c(1:3733)
data2=cbind(data2,r2)

s2=data2$r2[data2$V4==0]
row2=sample(s2,100)
o2=data2$r2[data2$V4==1]
sample2=rbind(data2[row2,],data2[o2,])

str(sample2)


#data prepration

table(sample2$V4)

##partition

radif2=c(1:157)
TrainRows2=sample(radif2,dim(sample2)[1]*0.7)
TestRows2=setdiff(radif2,TrainRows2)
TrainData2=sample2[TrainRows2,]
TestData2=sample2[TestRows2,]
names(TestData2)

#modeling

#GLM########################################################

fit2=glm(TrainData2$V4~factor(V1)+factor(V2)+factor(V3),data=TrainData2,family=binomial())
summary(fit2)

predict2=predict(fit2, newdata =TrainData2, type = "response")

pred2=ifelse(predict2 > 0.5,1,0)
length(TrainData2$V4)
length(pred2)
CM2=confusionMatrix(data=as.factor(pred2), reference =as.factor(TrainData2$V4))
CM2

predictTest2=predict(fit2, newdata =TestData2, type = "response")
predTest2=ifelse(predictTest2 > 0.5,1,0)
CMTest2=confusionMatrix(data=as.factor(predTest2), reference =as.factor(TestData2$V4))
CMTest2

#tree###########################################
# testtype: "Bonferroni","MonteCarlo","Univariate","Teststatistic" 
tree2=ctree(factor(TrainData2$V4)~factor(V1)+factor(V2)+factor(V3),data=TrainData2, 
            controls = ctree_control(teststat = "quad",
                                     testtype = "Univariate",
                                     mincriterion = 0.95,
                                     minsplit = 1,
                                     minbucket = 3,stump = FALSE,maxdepth=3 ))
tree2
plot(tree2)
predtree2=predict(tree2)
length(predict(tree2))
CMtree2=confusionMatrix(data=as.factor(predtree2), reference =as.factor(TrainData2$V4))
CMtree2


#tabTree1=table(predict(tree1),factor(TrainData1$V4))  
#tabTree1
#sensitivity1=tabTree1[1]/(tabTree1[1]+tabTree1[3])
#sensitivity1*100
#specificity1=tabTree1[4]/(tabTree1[2]+tabTree1[4])
#specificity1*100




