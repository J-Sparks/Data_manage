library(haven)
library(car)
library(survey)
library(dplyr)
library(stringr)
##################################
#### Survey DATA NHIS - EYE ######
##################################

library(haven)

NHIS2014TO2018DiabetesNew <- read_dta("C:/Users/jk99/Downloads/NHIS2014TO2018DiabetesNew.dta")
View(NHIS2014TO2018DiabetesNew)

Seen_Doc_eye_master <- select(NHIS2014TO2018DiabetesNew, "srvy_yr",contains("dibev"),
                              "sex","region","age_p","phstat","incgrp4",
                              "smkstat2","alcstat","hypev","hrtev","chlev","notcov","ahcsyr9",
                              "ausualpl","aplkind","ahcsyr2","avisexam","ppsu","wtfa","pstrat")



# creat a col name diavetes for 2016 and 2017 diavetes==1
Seen_Doc_eye_master$diabetes <- ifelse(Seen_Doc_eye_master$dibev1 == 1 | Seen_Doc_eye_master$dibev==1, 1,0)
Seen_Doc_eye_year201617 <- Seen_Doc_eye_master %>%
  select(-2,-3 ) %>% # omitted dibev1 and dibev cols
  filter(srvy_yr== 2016 | srvy_yr==2017) # filter for 2016 and 2017 year
#### select variables #########
all_seendoc_eye <- select(Seen_Doc_eye_year201617, "srvy_yr", "sex", "region","age_p",
                          "phstat","incgrp4",
                          "smkstat2","alcstat","hypev","hrtev","chlev","notcov","ahcsyr9",
                          "ausualpl","aplkind","ahcsyr2" , "ppsu", "wtfa","pstrat","avisexam")
colnames(all_seendoc_eye)

###########################################
#### Survey DATA NHIS - EYE - CODING ######
###########################################

###<labelled<double>[6331]>: Last time you had an eye exam (pupils dilated)
all_seendoc_eye$avisexam1<-as.factor(ifelse(all_seendoc_eye$avisexam <=2, "Less than 12 months",
                                            ifelse(all_seendoc_eye$avisexam == 3, "13-24 months",
                                                   ifelse(all_seendoc_eye$avisexam <= 5,"More than 2 years/Never", NA))))
all_seendoc_eye$avisexam1 <- relevel(all_seendoc_eye$avisexam1, ref = "13-24 months")
### level 5
all_seendoc_eye$avisexam5<-as.factor(ifelse(all_seendoc_eye$avisexam ==1 , "Less than one month",
                                            ifelse(all_seendoc_eye$avisexam == 2, "1-12 months",
                                                   ifelse(all_seendoc_eye$avisexam == 3,"13-24 months", 
                                                          ifelse(all_seendoc_eye$avisexam >= 5, "More than 2 years/Never",NA)))))
all_seendoc_eye$avisexam5 <- relevel(all_seendoc_eye$avisexam5, ref = "13-24 months")
### for in one year or not
all_seendoc_eye$avisexam2<-as.factor(ifelse(all_seendoc_eye$avisexam <=2, "Yes",
                                            ifelse(all_seendoc_eye$avisexam <= 5, "No", NA)))
### for three levels
all_seendoc_eye$avisexam3<-as.factor(ifelse(all_seendoc_eye$avisexam <=2, 1,
                                            ifelse(all_seendoc_eye$avisexam <= 3,2, ifelse(all_seendoc_eye$avisexam<=5,3,NA))))
all_seendoc_eye$avisexam3 <- relevel(all_seendoc_eye$avisexam3, ref = 2)
###<labelled<double>[6331]>: Ever been told you have hypertension
all_seendoc_eye$hypev1<-as.factor(ifelse(all_seendoc_eye$hypev ==1, "Yes",
                                         ifelse(all_seendoc_eye$hypev == 2, "No", NA)))
###<labelled<double>[6331]>: Ever been told you had a heart condition/disease
all_seendoc_eye$hrtev1<-as.factor(ifelse(all_seendoc_eye$hrtev ==1, "Yes",
                                         ifelse(all_seendoc_eye$hrtev == 2, "No", NA)))
###<labelled<double>[6331]>: Ever told you had high cholesterol
all_seendoc_eye$chlev1<-as.factor(ifelse(all_seendoc_eye$chlev ==1, "Yes",
                                         ifelse(all_seendoc_eye$chlev == 2, "No", NA)))

###<labelled<double>[6331]>: Seen/talked to a general doctor, past 12 m
all_seendoc_eye$ahcsyr91<-as.factor(ifelse(all_seendoc_eye$ahcsyr9 ==1, "Yes",
                                           ifelse(all_seendoc_eye$ahcsyr9 == 2, "No", NA)))
###<labelled<double>[6331]>: Place USUALLY go when sick
all_seendoc_eye$ausualpl1<-as.factor(ifelse(all_seendoc_eye$ausualpl <=1, "Yes","NO"))
###<labelled<double>[6331]>: Place to go when sick (most often)
all_seendoc_eye$aplkind1<-as.factor(ifelse(all_seendoc_eye$aplkind ==1, "Clinic or health center",ifelse(all_seendoc_eye$aplkind == 2, "Doctor's office or HMO",ifelse(all_seendoc_eye$aplkind==3,"Hospital emergency room",ifelse(all_seendoc_eye$aplkind==4,"Hospital outpatient department",ifelse(all_seendoc_eye$aplkind==5,"Some other place", NA))))))
### <labelled<double>[6331]>: Alcohol drinking status: Recode
all_seendoc_eye$alcstat1<-as.factor(ifelse(all_seendoc_eye$alcstat == 1, "No",
                                           ifelse(all_seendoc_eye$alcstat >= 2, "Yes",NA)))
###<labelled<double>[6331]>: Cov stat as used in Health United States
all_seendoc_eye$notcov1<-as.factor(ifelse(all_seendoc_eye$notcov ==1, "Not covered",
                                          ifelse(all_seendoc_eye$notcov == 2, "Covered", NA)))
###<labelled<double>[6331]>: Smoking Status: Recode
all_seendoc_eye$smkstat21<-as.factor(ifelse(all_seendoc_eye$smkstat2 ==1, "Current every day smoker",
                                            ifelse(all_seendoc_eye$smkstat2 == 2, "Current some day smoker",
                                                   ifelse(all_seendoc_eye$smkstat2 == 3,"Former smoker", "Never smoker"))))
all_seendoc_eye$smkstat21<-relevel(all_seendoc_eye$smkstat21, ref='Never smoker')
## recode for smkstat2
###<labelled<double>[6331]>: Smoking Status: Recode
all_seendoc_eye$smkstat22<-as.factor(ifelse(all_seendoc_eye$smkstat2 <=3, "Yes",
                                            ifelse(all_seendoc_eye$smkstat2 <= 4, "No", NA)))
all_seendoc_eye$smkstat22<-relevel(all_seendoc_eye$smkstat22, ref='No')
###<labelled<double>[6331]>: Total combined family income (grouped)
all_seendoc_eye$incgrp41<-as.factor(ifelse(all_seendoc_eye$incgrp4 ==1, "Under$34,999",
                                           ifelse(all_seendoc_eye$incgrp4 == 2, "$35,000 - $49,999",
                                                  ifelse(all_seendoc_eye$incgrp4 == 3,"$50,000 - $74,999",
                                                         ifelse(all_seendoc_eye$incgrp4==4,"$75,000 - $99,999", ifelse(all_seendoc_eye$incgrp4==5,"$100,000 and over",NA))))))
all_seendoc_eye$incgrp41 <- relevel(all_seendoc_eye$incgrp41, ref = "Under$34,999")
###<labelled<double>[6331]>: Sex
all_seendoc_eye$male<-as.factor(ifelse(all_seendoc_eye$sex ==1, "Male", "Female"))

###<labelled<double>[6331]>: Region
all_seendoc_eye$region1<-as.factor(ifelse(all_seendoc_eye$region ==1, "Northeast",
                                          ifelse(all_seendoc_eye$region == 2, "Midwest",
                                                 ifelse(all_seendoc_eye$region == 3,"South", "West"))))
all_seendoc_eye$region1 <- relevel(all_seendoc_eye$region1, ref="Northeast")
###<labelled<double>[6331]>: Age

#AGE_P Age of persons; values are 18-85 (85 includes age 85 and older)
all_seendoc_eye$age_p1<- cut(all_seendoc_eye$age_p, breaks =c(18,40,50,65,85)) # created NA
###
all_seendoc_eye$seendoc<-as.factor(ifelse(all_seendoc_eye$ahcsyr2 ==1, "Yes", "No"))
###<labelled<double>[6331]>: Seen/talked to eye doctor, past 12 m
all_seendoc_eye$phstat1 <-as.factor(ifelse(all_seendoc_eye$phstat ==1 | all_seendoc_eye$phstat ==2, "Excellent",
                                           ifelse(all_seendoc_eye$phstat == 3, "Good",
                                                  ifelse(all_seendoc_eye$phstat == 4 | all_seendoc_eye$phstat == 5,"fair/Poor", NA))))
###<labelled<double>[6331]>: Reported health status
all_seendoc_eye$phstat1 <- relevel(all_seendoc_eye$phstat1, ref = "fair/Poor")
### weights for 2016 and 2017
all_seendoc_eye$wtfa1 <- (all_seendoc_eye$wtfa)/2
# survy year
all_seendoc_eye$srvy_yr <- as.factor(all_seendoc_eye$srvy_yr)

#####################################################
#### Survey DATA NHIS - EYE - Descriptive STAT######
#####################################################

# Completed data SET $ descriptive STAT
sub <-all_seendoc_eye %>%
  select(ppsu,pstrat,wtfa1,srvy_yr, male, region1,age_p1, phstat1,incgrp41, 
         smkstat21,smkstat22,notcov1,hypev1,hrtev1,chlev1,ahcsyr91,ausualpl1,aplkind1,
         alcstat1,seendoc,avisexam1,avisexam2,avisexam3) %>%
  filter( complete.cases(.))
summary(sub)

#First we tell R our survey design
library(survey)
options(survey.lonely.psu = "adjust")
eye_nhis_design <-svydesign(ids=~ppsu, strata=~pstrat, weights=~wtfa1, data =sub , nest = TRUE)

######################################################################################
#### Survey DATA NHIS - EYE - Seen doc svyglm & Odd Ratios & CI by all variables ######
######################################################################################



########################################################
#### Survey DATA NHIS - EYE - logistic Regression ######
########################################################

#Full Logit model
fit.logit0all<-svyglm(seendoc ~ male + age_p1 + phstat1 + region1+incgrp41+smkstat22+
                        notcov1+hypev1+hrtev1+chlev1+ahcsyr91+ausualpl1+aplkind1+alcstat1,
                      design= eye_nhis_design,
                      family="binomial")
## Warning in eval(family$initialize): non-integer #successes in a binomial glm!
summary(fit.logit0all);exp(coef(fit.logit0all));round(confint(fit.logit0all),4)


#############################################################
#### Survey DATA NHIS - EYE - Multinomial Regression ######
#############################################################
## Multinomial Regression Analysis #1
library(nnet)
library(VGAM)

newmfit<-vglm(avisexam3~ age_p1 + region1+incgrp41+notcov1+hrtev1+chlev1+ahcsyr91,
              multinomial(refLevel = 2),sub,
              weights=wtfa1/mean(wtfa1, na.rm=T))
summary(newmfit)

round(exp(coef(newmfit)), 4) # odd ratios

# Prediction
p <- predict(newmfit, type="resp")
results <- cbind(sub, p)
results[1:10,]
AIC(newmfit)
