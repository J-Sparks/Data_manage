########################################
### Tables export the results ################
########################################

#need labraries
pkgs <- c("dplyr", "tidyr", "broom")
install.packages(pkgs) #install 
sapply(pkgs, require, character.only = T)



#for classification results
library(naivebayes)
allmodelNB <- naive_bayes(Drop1stYear ~ ., data = allDropoutTrainingSet, usekernel = TRUE)
#confusionMatrix
library(caret)
confMatRank <- table(predict(allmodelNB), allDropoutTrainingSet$Drop1stYear)
confusionMatrix(confMatRank)

t <-tables(allmodelNB) # error

### export the table to word
write.table(t, file="Dropout_Results.txt", sep=",", 
            quote=FALSE, row.names = F)

### export crosstab

t <- with(allDropout, table(ReStartMajor, ReFirstMathCrs ))
t <- prop.table(t, margin = 1)
t

write.table(t, file = "major_mathcrs.txt", sep=",",  quote=FALSE, row.names = T)#good
# go to txt file and copy all and paste in word and conver it as a table


### summary statistics
sumdropout <- allDropout %>% 
  # select variables and renames them (newname, oldname)
  #don't include_in variables name
  select("Prior(%)"=Prior_Hours,"GPAHS(%)"=GPA_HIGHSCHOOL,"UWFGPA1(%)"=UWFGPA1stTerm) %>% 
  #find mean, st,dev, min,and max for each variables
  summarise_each(funs(mean, sd, min,max)) %>% 
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable","stat"), sep = "_") %>% 
  spread(stat, value) %>% 
  #set order
  select(variable, mean, sd, min, max) %>% 
  mutate_each(funs(round(., 1)), -variable)
sumdropout
  
