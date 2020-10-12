################################################################
###  title: Tier 4 and 5 ; above or below average GPA      #####
################################################################


# Read data
library(caret)
library(naivebayes)
library(dplyr)
library(readr)
MASTER_CRSCSE_allcourse_tier <- read_csv("MASTER_CRSCSE_allcourse_tier.csv", 
                                         col_types = cols(X1 = col_skip())) #come with NA using readr
View(MASTER_CRSCSE_allcourse_tier)
glimpse(MASTER_CRSCSE_allcourse_tier)

#dealing with Na and repalce with OTHERS
df_first <- select(MASTER_CRSCSE_allcourse_tier, 1:19)
library(tidyr)
df_first <- data.frame(df_first, stringsAsFactors = FALSE)
head(df_first,10)
df_first[is.na(df_first)] <- "OTHERS" #"NO"

df_second <- select(MASTER_CRSCSE_allcourse_tier, 20:73)

V1wide_AllCSECRS_data_1 <- cbind(df_first,df_second);glimpse(V1wide_AllCSECRS_data_1)

write.csv(V1wide_AllCSECRS_data_1,"V1MASTER_Tiers_analysis.csv")


######################################
### Descriptive statistics ###########
######################################

# Ethnicity
library('data.table')
dfcollege <- data.table( college = V1wide_AllCSECRS_data_1$ENTRY_COLLEGE)
dfcollege[,.(count = .N), by = college][, percent := prop.table(count)*100][order(-count),][]

# Tier system
dftier <- data.table( Tiers = V1wide_AllCSECRS_data_1$APPLICANT_TIER)
dftier[,.(count = .N), by = Tiers][, percent := prop.table(count)*100][order(-count),][]
# Ethnicity
dfEthi <- data.table( Etnicity = V1wide_AllCSECRS_data_1$ETHNICITY)
dfEthi[,.(count = .N), by = Etnicity][, percent := prop.table(count)*100][order(-count),][]
# County
dfEcou <- data.table( county = V1wide_AllCSECRS_data_1$COUNTY)
dfEcou[,.(count = .N), by = county][, percent := prop.table(count)*100][order(-count),][]
# Majors
dfmajor <- data.table( majors = V1wide_AllCSECRS_data_1$ENTRY_PROGRAM)
dfmajor[,.(count = .N), by = majors][, percent := prop.table(count)*100][order(-count),][]
# department
dfdepart <- data.table( depart = V1wide_AllCSECRS_data_1$ENTRY_DEPARTMENT)
dfdepart[,.(count = .N), by = depart][, percent := prop.table(count)*100][order(-count),][]


# find average GPA
mean(V1wide_AllCSECRS_data_1$FIRST_FALL_GPA, na.rm=TRUE)#[1]  2.776109
mean(V1wide_AllCSECRS_data_1$HOURS_BROUGHT_TO_UNIVERSITY, na.rm=TRUE) # 14.27381

# Create new cols PriorInd and aveGPAInd
FALLGPA <- updated1CSEdata$FIRST_FALL_GPA
V1wide_AllCSECRS_data_1$FIRST_FALL_GPA <- FALLGPA
V2wide_AllCSECRS_data <- V1wide_AllCSECRS_data_1 %>% 
    mutate(PriorInd=ifelse(HOURS_BROUGHT_TO_UNIVERSITY<=0,"InexpFTIC","ExpFTIC")) %>% 
    mutate(aveGPAInd=ifelse(FIRST_FALL_GPA >=2.77,"above2.77","below2.77" )) 

#descriptive
wideV2_tier <- V2wide_AllCSECRS_data %>% 
  group_by(APPLICANT_TIER,aveGPAInd) %>% 
  summarise(mean_GPA = mean(FIRST_FALL_GPA),meanPrir=mean(HOURS_BROUGHT_TO_UNIVERSITY),cFTIC=n(),.groups="drop")
wideV2_tier # tier 4 and 5 
TD <- write.table(wideV2_tier,"wideV2_tier.csv", sep = ",")

wideV2_College <- V2wide_AllCSECRS_data %>% 
  group_by(ENTRY_COLLEGE,aveGPAInd) %>% 
  summarise(mean_GPA = mean(FIRST_FALL_GPA),meanPrir=mean(HOURS_BROUGHT_TO_UNIVERSITY),cFTIC=n(),.groups="drop")
wideV2_College # tier 4 and 5 
 write.table(wideV2_College,"wideV2_College.csv", sep = ",")
#Ethnicity
#descriptive
wideV2_eth <- V2wide_AllCSECRS_data %>% 
  group_by(ETHNICITY,HIGH_SCHOOL_NAME) %>% 
  summarise(mean_GPA = mean(FIRST_FALL_GPA),meanPrir=mean(HOURS_BROUGHT_TO_UNIVERSITY),cFTIC=n(),.groups="drop")
wideV2_eth # tier 4 and 5 

wideV2_hs <- V2wide_AllCSECRS_data %>% 
  filter(COUNTY_GROUP=="Tri-County") %>% #|COUNTY=="Santa Rosa"|COUNTY=="Okaloosa") %>%
  filter(ETHNICITY=="African American") %>% #|ETHNICITY=="White") %>% 
  group_by(APPLICANT_TIER,ENTRY_COLLEGE,aveGPAInd )%>% 
  summarise(mean_GPA = mean(FIRST_FALL_GPA),meanHS=mean(GPA_HIGHSCHOOL),cFTIC=n(),.groups="drop")
wideV2_hs # tier 4 and 5 

wideV2_hs_W <- V2wide_AllCSECRS_data %>% 
  filter(COUNTY_GROUP=="Tri-County") %>% #|COUNTY=="Santa Rosa"|COUNTY=="Okaloosa") %>%
  filter(ETHNICITY=="White") %>% #|ETHNICITY=="White") %>% 
  group_by(APPLICANT_TIER,ENTRY_COLLEGE,aveGPAInd )%>% 
  summarise(mean_GPA = mean(FIRST_FALL_GPA),meanHS=mean(GPA_HIGHSCHOOL),cFTIC=n(),.groups="drop")
wideV2_hs_W # tier 4 and 5 
#densityplot

V2wide_AllCSECRS_data %>% 
  select(HOURS_BROUGHT_TO_UNIVERSITY, FIRST_FALL_PELL_AMOUNT, FIRST_FALL_BRIGHT_FUTURES_AMOUNT) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")


## histogram plot
ggplot(data=V2wide_AllCSECRS_data, aes(x= HOURS_BROUGHT_TO_UNIVERSITY))+geom_density(aes(fill=aveGPAInd))#+facet_grid(aveGPAInd~.)

qplot(HOURS_BROUGHT_TO_UNIVERSITY, data=V2wide_AllCSECRS_data, geom = "histogram", fill=aveGPAInd,
      bins=10,ylab="Frequency",xlab="Prior hours", main="Piror Hours (UWF)") #5822



## box plot by tiers
V2wide_AllCSECRS_data$APPLICANT_TIER <- as.factor(V2wide_AllCSECRS_data$APPLICANT_TIER)
qplot(APPLICANT_TIER, FIRST_FALL_GPA, data=V2wide_AllCSECRS_data, geom=c("boxplot"),
      fill=APPLICANT_TIER, main="First Fall GPA UWF by Tier")
## ggplot
V2wide_AllCSECRS_data$APPLICANT_TIER <- as.numeric(V2wide_AllCSECRS_data$APPLICANT_TIER)
ggplot(data=V2wide_AllCSECRS_data, aes(x=APPLICANT_TIER))+geom_histogram(aes(fill=aveGPAInd),position = "dodge")

##################################################
########### ANOVA ###############################
################################################

## ANOVA by tier
aov_tiers <- aov(FIRST_FALL_GPA ~ APPLICANT_TIER, data = V2wide_AllCSECRS_data)
summary(aov_tiers) # results show that the difference exist
#Tukey Post Hoc test
tukey_tiers <- TukeyHSD(aov_tiers, "APPLICANT_TIER", ordered = TRUE)
tukey_tiers
plot(tukey_tiesr)
plot(TukeyHSD(aov_tiers,"APPLICANT_TIER")) # shows the mean between tiers differ

# ANOVA by ETHNICITY
aov_ETHNICITYT <- aov(FIRST_FALL_GPA ~ ETHNICITY, data = V2wide_AllCSECRS_data)
summary(aov_ETHNICITYT)
#V2wide_AllCSECRS_dataHMCSE <- V2wide_AllCSECRS_data %>% filter(ENTRY_COLLEGE=="HMCSE")
aov_ETHNICITYTier <- aov(FIRST_FALL_GPA ~ ETHNICITY+APPLICANT_TIER+APPLICANT_TIER*ETHNICITY, data = V2wide_AllCSECRS_data)
summary(aov_ETHNICITYTier) 


tukey_enthnicity<- TukeyHSD(aov_ETHNICITY, "ETHNICITY", ordered = TRUE)
tukey_enthnicity # shows difference AA vs. others
qplot(ETHNICITY, FIRST_FALL_GPA, data =V2wide_AllCSECRS_data, geom = "boxplot",
      fill=ETHNICITY, main="First Fall GPA by ETHNICITY")

##ggplot
ggplot(data = V2wide_AllCSECRS_data ) + geom_bar(mapping = aes(x=ETHNICITY, fill=aveGPAInd), position = "dodge")
ggplot(data = V2wide_AllCSECRS_data ) + geom_bar(mapping = aes(x=APPLICANT_TIER, fill=ETHNICITY), position = "dodge")

###############################################
### Tiers @ HMCSE by Majors###################
## Naive Bayes Classifier ####################
###############################################



##############################################
########## CODING ############################
#############################################
#select original datset and code levels
aveGPANBV1 <- V2wide_AllCSECRS_data %>% 
  select("UNIV_ROW_ID","APPLICANT_TIER", "ENTRY_COLLEGE", "ENTRY_DEPARTMENT","AP_CREDITS","COUNTY","GENDER", "ENTRY_PROGRAM" ,"HIGH_SCHOOL_NAME","ETHNICITY",
                   "CRSNAME1",  "CRSNAME2",  "CRSNAME3",  "CRSNAME4", 
                  # "CRSNAME5",  "CRSNAME6",
                   "HOURS_BROUGHT_TO_UNIVERSITY","FIRST_FALL_PELL_AMOUNT","FIRST_FALL_BRIGHT_FUTURES_AMOUNT","AGE_AT_ENTRY",
                   "aveGPAInd") %>% 
mutate(NEWCOUNTY=ifelse(COUNTY == "Escambia","ESCA", ifelse(COUNTY=="Santa Rosa","SANTAROSA",ifelse(COUNTY=="Okaloosa","OKAL",ifelse(COUNTY=="Non-Florida","NONFL","OTHERS"))))) %>% 
mutate(NEWETHNICITY=ifelse(ETHNICITY=="White","White",
                           ifelse(ETHNICITY=="Hispanic","Hispanic",
                                  ifelse(ETHNICITY=="African American","AA",
                                         ifelse(ETHNICITY=="Two or More","Two/More",
                                                ifelse(ETHNICITY=="Asian","Asian","OTHERS"))))))  
       
       
## FACTOR ################################       
aveGPANB$APPLICANT_TIER <- as.factor(aveGPANB$APPLICANT_TIER)
aveGPANB$aveGPAInd <- as.factor(aveGPANB$aveGPAInd)
aveGPANB$GENDER <- as.factor(aveGPANB$GENDER)
aveGPANB$NEWETHNICITY <- as.factor(aveGPANB$NEWETHNICITY)
aveGPANB$CRSNAME1 <- as.factor(aveGPANB$CRSNAME1)
aveGPANB$CRSNAME2 <- as.factor(aveGPANB$CRSNAME2)
aveGPANB$CRSNAME3 <- as.factor(aveGPANB$CRSNAME3)
aveGPANB$CRSNAME4 <- as.factor(aveGPANB$CRSNAME4)
aveGPANB$HIGH_SCHOOL_NAME <- as.factor(aveGPANB$HIGH_SCHOOL_NAME)
aveGPANB$NEWCOUNTY <- as.factor(aveGPANB$NEWCOUNTY)
aveGPANB$ENTRY_COLLEGE <- as.factor(aveGPANB$ENTRY_COLLEGE)

########## FILTERING GROUPS ###################
aveGPANB <- aveGPANBV1 %>% 
  filter(ENTRY_COLLEGE=="HMCSE") %>%  # FTIC2019~
  #filter(ETHNICITY=="White"|ETHNICITY=="African American") %>% 
  filter(APPLICANT_TIER <= 1) #%>% 
  #na.omit()
#write.csv(aveGPANB,"aveGPANB.csv")
glimpse(aveGPANB)
summary(aveGPANB)

library(rsample)  # data splitting 
library(dplyr)    # data transformation
library(ggplot2)  # data visualization
library(caret)    # implementing with caret
library(h2o)      # implementing with h2o
# Performs stratified random split of the data set
set.seed(1234)
TrainingIndex <- createDataPartition(aveGPANB$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- aveGPANB[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- aveGPANB[-TrainingIndex,] # Test Set
write.csv(aveGPAIndtTrainingSet, "training.csv")
write.csv(aveGPAIndTestingSet, "testing.csv")
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1] #row number

#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

# Build model using all factors and labels
library(naivebayes)
library(randomForest)
aveGPANB_model <- naive_bayes(aveGPAInd ~ APPLICANT_TIER+GENDER+ETHNICITY+HIGH_SCHOOL_NAME+ENTRY_PROGRAM+AGE_AT_ENTRY+#NEWCOUNTY+ENTRY_COLLEGE+
                                CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4+
                              HOURS_BROUGHT_TO_UNIVERSITY+AP_CREDITS+
                                FIRST_FALL_PELL_AMOUNT+FIRST_FALL_BRIGHT_FUTURES_AMOUNT, data = aveGPAIndtTrainingSet, usekernel = TRUE)
confusionMatrix(predict(aveGPANB_model), aveGPAIndtTrainingSet$aveGPAInd) 
#0.7678, alltiers 0.6971 tier2:5 0.7036 3:5 0.7678 4:5 0.7862 5 0.7769
#backward <=4 0.72 <=3 0.7469 <=2 0.8262 <=1 0.9653
tables(aveGPANB_model)

#accuracy ..... used kernel
confMatRank <- table(predict(aveGPANB_model), aveGPAIndtTrainingSet$aveGPAInd)
confMatRank
sum(diag(confMatRank)/sum(confMatRank)) 
1-sum(diag(confMatRank)/sum(confMatRank))

#TEST dataset
aveGPANB_modelt <- naive_bayes(aveGPAInd ~ APPLICANT_TIER+GENDER+ETHNICITY+HIGH_SCHOOL_NAME+ENTRY_PROGRAM+AGE_AT_ENTRY+#NEWCOUNTY+ENTRY_COLLEGE+
                                 CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4+
                                 HOURS_BROUGHT_TO_UNIVERSITY+AP_CREDITS+
                                 FIRST_FALL_PELL_AMOUNT+FIRST_FALL_BRIGHT_FUTURES_AMOUNT, data = aveGPAIndTestingSet, usekernel = TRUE)
confusionMatrix(predict(aveGPANB_modelt), aveGPAIndTestingSet$aveGPAInd) 
#<=1 0.9178         
confMatRankt <- table(predict(aveGPANB_modelt), aveGPAIndTestingSet$aveGPAInd)
confMatRankt
sum(diag(confMatRankt)/sum(confMatRankt)) 
1-sum(diag(confMatRankt)/sum(confMatRankt)) 

#without dependent
#x <- aveGPAIndTestingSet[,-13];glimpse(x)
ptest <-  predict(aveGPANB_modelt, aveGPAIndTestingSet, type="prob")
Result.NB.aveGPAV5 <-  cbind(aveGPAIndTestingSet,ptest)
write.csv(Result.NB.aveGPAV5, "V5Result.NB.aveGPA.csv") # test data for tier 1



# export results
p <- predict(aveGPANB_model, aveGPAIndtTrainingSet, type="prob")
Result.NB.aveGPAV3 <-  cbind(aveGPAIndtTrainingSet,p)
write.csv(Result.NB.aveGPAV3, "V4Result.NB.aveGPA.csv") # train data for tier 1


pred <- predict(aveGPANB_model, aveGPAIndtTrainingSet[c(1:10),], type="prob")
pred
n <- table(aveGPAIndtTrainingSet$aveGPAInd)
n
n/sum(n)

########### descriptive ############################
#correlation
aveGPAIndtTrainingSet %>%
  select(-1) %>% 
  filter(aveGPAInd == "above2.77") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

#normality density
aveGPAIndtTrainingSet %>% 
  select(HOURS_BROUGHT_TO_UNIVERSITY, FIRST_FALL_PELL_AMOUNT, FIRST_FALL_BRIGHT_FUTURES_AMOUNT) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")
unique(aveGPAIndtTrainingSet$COUNTY)

###  overfitting issue
# using LOOCV RESAMPLING
model.loocv <- train(aveGPAInd ~ APPLICANT_TIER+GENDER+NEWETHNICITY+NEWCOUNTY+#CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4+
                       HOURS_BROUGHT_TO_UNIVERSITY+AP_CREDITS+FIRST_FALL_PELL_AMOUNT+FIRST_FALL_BRIGHT_FUTURES_AMOUNT, data = aveGPAIndtTrainingSet,
                     method="rf", TuneLength=3,trControl=trainControl(method = "LOOCV", number=10,classProbs = TRUE))

# using K-folder corss validation resampling
model.cv <- train(aveGPAInd ~ APPLICANT_TIER+GENDER+NEWETHNICITY+NEWCOUNTY+#CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4+
  HOURS_BROUGHT_TO_UNIVERSITY+AP_CREDITS+FIRST_FALL_PELL_AMOUNT+FIRST_FALL_BRIGHT_FUTURES_AMOUNT, data = aveGPAIndtTrainingSet,
method="rf", TuneLength=3,trControl=trainControl(method = "cv", number=10,classProbs = TRUE)) 

# using bootstrapping resampling
model.boot <-train(aveGPAInd ~ APPLICANT_TIER+GENDER+NEWETHNICITY+NEWCOUNTY+#CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4+
                     HOURS_BROUGHT_TO_UNIVERSITY+AP_CREDITS+FIRST_FALL_PELL_AMOUNT+FIRST_FALL_BRIGHT_FUTURES_AMOUNT, data = aveGPAIndtTrainingSet, 
                   method="rf", TuneLength=3,trControl=trainControl(method = "boot", number=10,classProbs = TRUE)) 

# using repeated k-fold cross validation resampling
model.repeatedcv <- train(aveGPAInd ~ APPLICANT_TIER+GENDER+NEWETHNICITY+NEWCOUNTY+#CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4+
                            HOURS_BROUGHT_TO_UNIVERSITY+AP_CREDITS+FIRST_FALL_PELL_AMOUNT+FIRST_FALL_BRIGHT_FUTURES_AMOUNT, data = aveGPAIndtTrainingSet, 
                          method="rf", TuneLength=3,trControl=trainControl(method = "repeatedcv", number=10,classProbs = TRUE)) 

#results
model.loocv$results
model.cv$results
model.boot$results
model.repeatedcv$results



# prediction
ploocv <- predict(model.loocv, aveGPATrainingSet, "prob")
pcv <- predict(model.cv, aveGPATrainingSet, "prob")
pboot <- predict(model.boot, aveGPATrainingSet, "prob")
prepeatedcv <- predict(model.repeatedcv, aveGPATrainingSet, "prob")

confusionMatrix(ploocv, aveGPATrainingSet$aveGPA)#0.8107
confusionMatrix(pcv, aveGPATrainingSet$aveGPA)#0.8107
confusionMatrix(pboot, aveGPATrainingSet$aveGPA)#0.8107
confusionMatrix(prepeatedcv, aveGPATrainingSet$aveGPA)#0.8107



###########################################
######## APR ##############################
###########################################
# Build model using all factors and labels
#select original datset
aveGPANB <- select(V3wide_AllCSECRS_data_tier4and5,  "GENDER", "ENTRY_PROGRAM" ,"ETHNICITY","APPLICANT_TIER","CRSNAME1",  "CRSNAME2",  "CRSNAME3",  "CRSNAME4", 
                   # "CRSNAME5",  "CRSNAME6",
                   "HOURS_BROUGHT_TO_UNIVERSITY","FIRST_FALL_PELL_AMOUNT","FIRST_FALL_BRIGHT_FUTURES_AMOUNT",
                   "APR")
library(caret)
# Performs stratified random split of the data set
set.seed(1234)
TrainingIndex <- createDataPartition(aveGPANB$aveGPAInd, p=0.8, list = FALSE)
aveGPAIndtTrainingSet <- aveGPANB[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- aveGPANB[-TrainingIndex,] # Test Set
aveGPANB_modelAPR <- naive_bayes(APR ~ ., data = aveGPAIndtTrainingSet)
tables(aveGPANB_modelAPR)


#aveGPANB_model.cv <- train(aveGPAInd ~ ., data = aveGPAIndtTrainingSet,
#                           +                 method = "nb",
#                           +                 preProcess = c("center", "scale"),
#                           +                 tuneLength = 3,
#                           +                 trControl = trainControl(method = "LOOCV"))


#accuracy ..... used kernel
confMatRank <- table(predict(aveGPANB_modelAPR), aveGPAIndtTrainingSet$APR)
confMatRank
sum(diag(confMatRank)/sum(confMatRank)) # 0.7025948 dropped accuracy using APR
1-sum(diag(confMatRank)/sum(confMatRank))

############################################
### Inexperienced FTIC @ HMCSE #############
############################################


V3wide_AllCSECRS_data_InexpFTIC <- V2wide_AllCSECRS_data %>% 
  filter(ENTRY_COLLEGE=="HMCSE") %>% 
  filter(PriorInd == "InexpFTIC") %>% 
  select(GENDER,contains("CRSNAME"),APPLICANT_TIER,
         ETHNICITY,
         FIRST_FALL_PELL_AMOUNT,FIRST_FALL_BRIGHT_FUTURES_AMOUNT,aveGPAInd) %>% 
  na.omit()


glimpse(V3wide_AllCSECRS_data_InexpFTIC)


#select original datset
aveGPANB_inecpFTIC <- select(V3wide_AllCSECRS_data_InexpFTIC,  "GENDER",    "CRSNAME1",  "CRSNAME2",  "CRSNAME3",  "CRSNAME4", 
                   # "CRSNAME5",  "CRSNAME6",
                  # "HOURS_BROUGHT_TO_UNIVERSITY" ,
                  "APPLICANT_TIER",
                   "ETHNICITY","FIRST_FALL_PELL_AMOUNT","FIRST_FALL_BRIGHT_FUTURES_AMOUNT",
                   "aveGPAInd")
write.csv(aveGPANB_inecpFTIC,"aveGPANB_inecpFTIC.csv")

aveGPANB_inecpFTIC$aveGPAInd <- as.factor(aveGPANB_inecpFTIC$aveGPAInd)
aveGPANB_inecpFTIC$APPLICANT_TIER <- as.factor(aveGPANB_inecpFTIC$APPLICANT_TIER)
aveGPANB_inecpFTIC$GENDER <- as.factor(aveGPANB_inecpFTIC$GENDER)
aveGPANB_inecpFTIC$ETHNICITY <- as.factor(aveGPANB_inecpFTIC$ETHNICITY)
aveGPANB_inecpFTIC$CRSNAME1 <- as.factor(aveGPANB_inecpFTIC$CRSNAME1)
aveGPANB_inecpFTIC$CRSNAME2 <- as.factor(aveGPANB_inecpFTIC$CRSNAME2)
aveGPANB_inecpFTIC$CRSNAME3 <- as.factor(aveGPANB_inecpFTIC$CRSNAME3)
aveGPANB_inecpFTIC$CRSNAME4 <- as.factor(aveGPANB_inecpFTIC$CRSNAME4)
#aveGPANB$CRSNAME1 <- as.factor(aveGPANB$CRSNAME1)


summary(aveGPANB_inecpFTIC)

library(caret)
# Performs stratified random split of the data set
set.seed(1234)
TrainingIndex <- createDataPartition(aveGPANB_inecpFTIC$aveGPAInd, p=0.7, list = FALSE)
aveGPANB_inecpFTICtTrainingSet <- aveGPANB_inecpFTIC[TrainingIndex,] # Training Set
aveGPANB_inecpFTICTestingSet <- aveGPANB_inecpFTIC[-TrainingIndex,] # Test Set
write.csv(aveGPANB_inecpFTICtTrainingSet, "training.csv")
write.csv(aveGPANB_inecpFTICTestingSet, "testing.csv")
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1] #row number

# Build model using all factors and labels
library(naivebayes)
aveGPANBInexp_model <- naive_bayes(aveGPAInd ~ ., data = aveGPANB_inecpFTICtTrainingSet, usekernel = TRUE)
tables(aveGPANBInexp_model)
#accuracy ..... used kernel
confMatRank <- table(predict(aveGPANBInexp_model), aveGPANB_inecpFTICtTrainingSet$aveGPAInd)
confMatRank
sum(diag(confMatRank)/sum(confMatRank)) #   0.7853659 to 0.7833333
1-sum(diag(confMatRank)/sum(confMatRank)) 
p <- predict(aveGPANBInexp_model, aveGPANB_inecpFTICtTrainingSet)
pred <- predict(aveGPANBInexp_model, aveGPANB_inecpFTICtTrainingSet[c(1:10),], type="prob")
pred
n <- table(aveGPANB_inecpFTICtTrainingSet$aveGPAInd)
n
n/sum(n)





##################################################################
### Tier 4 and 5 FTIC @ HMCSE using association analysis#############
##################################################################
library(arules)
library(arulesViz)
V3wide_AllCSECRS_data_tier4and5 <- read.csv("~/Data_IR/V3wide_AllCSECRS_data_tier4and5.csv", stringsAsFactors=TRUE)
V4wide_AllCSECRS_data_tier4and5 <- V3wide_AllCSECRS_data_tier4and5 %>% 
  select(2:6,9:13);glimpse(V4wide_AllCSECRS_data_tier4and5)

table(V4wide_AllCSECRS_data_tier4and5$aveGPAInd) 
V4wide_AllCSECRS_data_tier4and5 <- as(V4wide_AllCSECRS_data_tier4and5, "transactions")

AllCSECRS_dataV1 <- V2wide_AllCSECRS_data %>% 
  filter(APPLICANT_TIER>= 4) %>% 
    select(2:5,"GENDER","ETHNICITY","aveGPAInd")

AllCSECRS_dataV2 <- as(AllCSECRS_dataV1, "transactions")

  
itemFrequencyPlot(AllCSECRS_dataV2, topN=25)
itemFrequencyPlot(AllCSECRS_dataV2, support=0.1)

rules_TierBelow <- apriori(AllCSECRS_dataV2,parameter = list(minlen=1, maxlen=8, conf=.1), appearance = list(rhs=c("aveGPAInd=below2.77"), 
                                                                                                                     default = "lhs"))

 inspectDT(head(sort(rules_TierBelow, by="lift"),90))
 

rules_TierAbove <- apriori(AllCSECRS_dataV2,parameter = list(minlen=1, maxlen=8, conf=.1), appearance = list(rhs=c("aveGPAInd=above2.77"), 
                                                                                                                       default = "lhs"))
inspectDT(head(sort(rules_TierAbove, by="lift"),36))

plot(x=rules_TierBelow,
     measure = c("confidence","lift"), shading = "support")
plot(x=rules_TierAbove,
     measure = c("confidence","lift"), shading = "support")

plot(rules_TierBelow, method="graph", engine = "interactive",shading = "lift") #grouped





