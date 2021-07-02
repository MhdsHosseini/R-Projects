data<-read.table(file.choose(),header=T)
data

#install.packages("unbalanced")#
library(unbalanced)
attach(data)



MM1=as.factor(M1)
MM2=as.factor(M2)
MM3=as.factor(M3)
MM6=as.factor(M6)
MM7=as.factor(M7)
MM8=as.factor(M8)
table(MM8)
str(data)

n <- ncol(data)
n
output <- data[ ,n]
output

class=(as.factor(output))
as.factor(output)



input <- data[ ,-n]
input


set.seed(1234)

#apply smot


#rebalance the training set before building a model
balanced <- ubBalance(X=input, Y=as.factor(output), type="ubSMOTE")
balfire <- data.frame(balanced$X, Class=balanced$Y)
summary(balfire$Class)


###################DT##################################################
attach(balfire)
names(balfire)

MM1=as.factor(M1)
MM2=as.factor(M2)
MM3=as.factor(M3)
MM6=as.factor(M6)
MM7=as.factor(M7)
library(party)

# be jaye testtype: "Bonferroni","MonteCarlo","Univariate","Teststatistic" 

tree=ctree(Class~MM1+MM2+MM3+M4+M5+MM6+MM7, 
  controls = ctree_control(teststat = "quad",
  testtype = "Bonferroni",
  mincriterion = 0.95,
  minsplit = 1,
  minbucket = 3,stump = FALSE,maxdepth=3 ))
tree
plot(tree)
tabTree=table(predict(tree),Class)  
tabTree
tabTree[3]
sensitivity=tabTree[1]/(tabTree[1]+tabTree[3])
sensitivity*100
specificity=tabTree[4]/(tabTree[2]+tabTree[4])
specificity*100

print(tabTree)
# deghat:
sum(diag(tabTree))/sum(tabTree)



#######################SVM##############################################
library(e1071)
wts <- 100 / table(Class)
wts
svmmodel=svm(Class~M1+M2+M3+M4+M5+M6+M7,
	controls = ctree_control(
	kernel ="radial",
	degree = 10,
	tolerance = 0.001, 
	epsilon = 0.1),class.weights =wts)

tabSvm=table(predict(svmmodel),Class)
tabSvm
print(tabSvm)

sensitivity=tabSvm[1]/(tabSvm[1]+tabSvm[3])
sensitivity*100
specificity=tabSvm[4]/(tabSvm[2]+tabSvm[4])
specificity*100

# deghate model:
sum(diag(tabSvm))/sum(tabSvm)



