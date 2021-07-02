data=read.table(file.choose(),header=T)
str(data)
attach(data)
M1F=as.factor(M1)
M2F=as.factor(M2)
M3F=as.factor(M3)
M6F=as.factor(M6)
M7F=as.factor(M7)
M8F=as.factor(M8)

###################DT##################################################
library(party)
# be jaye testtype: "Bonferroni","MonteCarlo","Univariate","Teststatistic" 
tree=ctree(M8F~M1F+M2F+M3F+M4+M5+M6F+M7F, data,
	controls = ctree_control(teststat = "quad",
	testtype = "Bonferroni",
	mincriterion = 0.85,
	minsplit = 1,
	minbucket = 3))
plot(tree)
tabTree=table(predict(tree),M8F)	
print(tabTree)
# deghat:
sum(diag(tabTree))/sum(tabTree)
#######################SVM##############################################
library(e1071)

##na.rm = TRUE
svmmodel=svm(M8F~M1F+M2F+M3F+M4+M5+M6F+M7F,
	controls = ctree_control(
	kernel ="radial",
	degree = 10,
	tolerance = 0.001, 
	epsilon = 0.1))

tabSvm=table(predict(svmmodel),M8F)
print(tabSvm)
# deghate model:
sum(diag(tabSvm))/sum(tabSvm)
