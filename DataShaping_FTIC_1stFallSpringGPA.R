library(tidyr)
library(dplyr)
library(stringr)
FTIC_1stTerm_ALL_DF <- STU_ENROLLMENT_SPRING20 %>% 
  filter(Stu_StudentTypeCode == "B" & # for FTIC
           Stu_LoadIPEDSFTPT=="Full Time" & # full time
           Stu_AdmissionRecentTypeCode=="B" & # FTIC
           Stu_DEMO_TIME_FRAME >= 201505 ) %>% # data quality better since 2015
  group_by(STU_ID) %>% 
  filter(str_detect(Stu_Term,"Fall...")) %>% 
  separate(Stu_AdmissionTerm, into = c("Term","Cohort"),sep=" ")



## choose variables for loaded crs hrs and totalgradepoints and 1st term GPA
FTIC_1stTerm_ALL_select <- FTIC_1stTerm_ALL_DF %>% 
  select(1:3,Cohort,Stu_Age,Stu_DivisionCode,Stu_MajorDesc,Stu_Gender,Stu_Ethnicity,Stu_FeeResidency,Stu_County,Stu_State,Stu_Nation,Stu_AdmissionRecentTypeDesc,Stu_LastInstitutionDesc,Stu_AdmissionTermCode,contains("GPA"),Stu_College,Stu_Department,contains("Total"),Stu_CurrentTermLoad) %>% 
  mutate(GPA1stFall=Stu_GPAGradePoints/Stu_GPATermHours)
#Nas to zero values
GPA1stFall_na <- FTIC_1stTerm_ALL_select$GPA1stFall
GPA1stFall_na[is.na(GPA1stFall_na)] <- 0
FTIC_1stTerm_ALL_select$GPA1stFall <- GPA1stFall_na
FTIC_1stTerm_ALL_select$rankGPA1stFall <- ifelse(FTIC_1stTerm_ALL_selectSpring$GPA1stFall<1.0,"Under1.0",
                                                         ifelse(FTIC_1stTerm_ALL_selectSpring$GPA1stFall<1.5,"under1.5",
                                                                ifelse(FTIC_1stTerm_ALL_selectSpring$GPA1stFall<2.0,"under2.0","above2.0")))
xtabs(~FTIC_1stTerm_ALL_select$Cohort+FTIC_1stTerm_ALL_select$rankGPA1stFall)
########################################
## to get Spring semester GPA for FTIC #
########################################

library(stringr)
FTIC_1stTerm_ALL_DFspring <- STU_ENROLLMENT_SPRING20 %>% 
  filter(  Stu_AdmissionRecentTypeCode=="B" & # FTIC
             Stu_DEMO_TIME_FRAME >= 201601 ) %>% # data quality better since 201601
  group_by(STU_ID) %>% 
  arrange(Stu_DEMO_TIME_FRAME, .by_group=TRUE) %>% 
  filter(str_detect(Stu_Term,"Spring...")) %>% 
  separate(Stu_AdmissionTerm, into = c("Term","Cohort"),sep=" ") %>% 
  filter(Cohort >= 2015) %>% 
  filter(Stu_DEMO_TIME_FRAME== min(Stu_DEMO_TIME_FRAME)) %>% 
  filter(Stu_StudentTypeCode=="C")

FTIC_1stTerm_ALL_selectSpring <- FTIC_1stTerm_ALL_DFspring %>% 
  select(1:3,Cohort,Stu_Age,Stu_DivisionCode,Stu_MajorDesc,Stu_Gender,Stu_Ethnicity,Stu_FeeResidency,Stu_County,Stu_State,Stu_Nation,Stu_AdmissionRecentTypeDesc,Stu_LastInstitutionDesc,Stu_AdmissionTermCode,contains("GPA"),Stu_College,Stu_Department,contains("Total"),Stu_CurrentTermLoad) %>% 
  mutate(GPA1stSpring=Stu_GPAGradePoints/Stu_GPATermHours)
#Nas to zero values
GPA1stSpring_na <- FTIC_1stTerm_ALL_selectSpring$GPA1stSpring
GPA1stSpring_na[is.na(GPA1stSpring_na)] <- 0
FTIC_1stTerm_ALL_selectSpring$GPA1stSpring <- GPA1stSpring_na
FTIC_1stTerm_ALL_selectSpring$rankGPA1stSpring <- ifelse(FTIC_1stTerm_ALL_selectSpring$GPA1stSpring<1.0,"Under1.0",
                                                         ifelse(FTIC_1stTerm_ALL_selectSpring$GPA1stSpring<1.5,"under1.5",
                                                                ifelse(FTIC_1stTerm_ALL_selectSpring$GPA1stSpring<2.0,"under2.0","above2.0")))
xtabs(~FTIC_1stTerm_ALL_selectSpring$Cohort+FTIC_1stTerm_ALL_selectSpring$rankGPA1stSpring)

########################################################
## to merge with Fall and Spring semester GPA for FTIC #
########################################################

FTIC_Fall_Spring_GPA <- merge(FTIC_1stTerm_ALL_selectSpring,FTIC_1stTerm_ALL_select, by="STU_ID", all.y= TRUE)
