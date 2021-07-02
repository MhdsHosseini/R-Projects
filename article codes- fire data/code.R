data=read.table(file.choose(),header=T)
str(data)
attach(data)
M1F=as.factor(M1)
M2F=as.factor(M2)
M3F=as.factor(M3)
M6F=as.factor(M6)
M7F=as.factor(M7)
M8F=as.factor(M8)



M1F
###################DT##################################################
install.packages("party")
library(party)
# be jaye testtype: "Bonferroni","MonteCarlo","Univariate","Teststatistic" 
?ctree
tree=ctree(M8F~M1F+M2F+M3F+M4+M5+M6F+M7F, data,
	controls = ctree_control(teststat = "quad",
	testtype = "Teststatistic",
	mincriterion = 0.85,
	minsplit = 1,
	minbucket = 3))
tree
plot(tree)
tabTree=table(predict(tree),M8F)	
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

##na.rm = TRUE

table(M8F)
wts <- 100 / table(M8F)
wts
svmmodel=svm(M8F~M1F+M2F+M3F+M4+M5+M6F+M7F,
	controls = ctree_control(
	kernel ="radial",
	degree = 10,
	tolerance = 0.001, 
	epsilon = 0.1),class.weights =wts)

tabSvm=table(predict(svmmodel),M8F)
tabSvm
print(tabSvm)

sensitivity=tabSvm[1]/(tabSvm[1]+tabSvm[3])
sensitivity*100
specificity=tabSvm[4]/(tabSvm[2]+tabSvm[4])
specificity*100




# deghate model:
sum(diag(tabSvm))/sum(tabSvm)
