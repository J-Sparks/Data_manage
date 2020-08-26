#############################
### ACCURECY ################
#############################

# Using Navie Bayes classification

#Confusion Matrix (act\pred)
confMatRank <- table(predict(allmodelNB), allDropoutTrainingSet$Drop1stYear)
confMatRank 

#Accurecy
sum(diag(confMatRank)/sum(confMatRank)) # 0.8438356

# misclassfication
1-sum(diag(confMatRank)/sum(confMatRank)) 

# Predition with type = probability
p <- predict(allmodelNB, allDropoutTrainingSet)
pred <- predict(allmodelNB, allDropoutTrainingSet[c(1:10),], type="prob")
pred

#prior fre and prob
n <- table(allDropoutTrainingSet$Drop1stYear)
n
n/sum(n)