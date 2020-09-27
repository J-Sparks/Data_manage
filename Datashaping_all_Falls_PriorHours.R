#################################################
#### Association Rule for prior hours == 0 ######
#################################################


#### getting 1st Fall  data ready
library(readr)
STU_ENROLLMENT_SPRING20 <- read_csv("C:/Users/jsparks3/Downloads/drive-download-20200901T135512Z-001/STU_ENROLLMENT_SPRING20.csv")


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
  select(1:3,Cohort,Stu_Age,Stu_DivisionCode,Stu_MajorDesc,Stu_Gender,Stu_Ethnicity,Stu_FeeResidency,Stu_County,Stu_State,Stu_Nation,
         Stu_AdmissionRecentTypeDesc,Stu_AdmissionTermCode,contains("GPA"),Stu_College,Stu_Department,
         contains("Total"),Stu_CurrentTermLoad,Stu_APInd,Stu_ResidenceHall) %>% 
  mutate(GPA1stFall=Stu_GPAGradePoints/Stu_GPATermHours) 
#Nas to zero values
GPA1stFall_na <- FTIC_1stTerm_ALL_select$GPA1stFall
GPA1stFall_na[is.na(GPA1stFall_na)] <- 0
FTIC_1stTerm_ALL_select$GPA1stFall <- GPA1stFall_na # 2.772962 UWF 1st fall average B- ==2.7

#clean data
FTIC_1stTerm_ALL_selectDF <- FTIC_1stTerm_ALL_select %>%
  mutate(PriorUniAPR = ifelse(Stu_TotalUniversityHours<=0,"NoPriorHrs","YesPriorHrs")) %>% 
  mutate(PriorUWFAPR = ifelse(Stu_TotalInstHours<=0,"NoUWFPriorHrs","YesUWFPriorHrs")) %>% 
  mutate(APRGPA1stFall= ifelse(GPA1stFall>= 2.77,"Above2.77","Below2.77")) 


FTIC_1stTerm_ALL_selectDF$RankGPA1stFall <- ifelse(GPA1stFall<1.0,"under1.0", #Below average D
                                                   ifelse(GPA1stFall<1.7,"under1.7", #average C-
                                                          ifelse(GPA1stFall<2.0,"under2.0", #UWF APR GPA 2.0 average C
                                                                 ifelse(GPA1stFall<2.77,"under2.77","above2.77"))))
 

glimpse(FTIC_1stTerm_ALL_selectDF)
#Compare with APR GPA 
FTIC_GPA_com <- FTIC_1stTerm_ALL_selectDF %>% 
  group_by(PriorUniAPR,APRGPA1stFall) %>% 
  summarise(meanGPA = mean(GPA1stFall),meanTermHrs=mean(Stu_GPATermHours),meanPriorHrs=mean(Stu_TotalUniversityHours),CountFTIC=n())  #(meanGPA = mean(GPA1stFall),
FTIC_GPA_com



#########################################
#### GET all Fall university hours ######
#########################################
##DATA1
FTIC_ID_from_fall <- STU_ENROLLMENT_SPRING20 %>% 
  filter(Stu_StudentTypeCode == "B" & # for FTIC
           Stu_LoadIPEDSFTPT=="Full Time" & # full time
           Stu_AdmissionRecentTypeCode=="B" & # FTIC
           Stu_DEMO_TIME_FRAME >= 201505 ) %>% # data quality better since 2015
  group_by(STU_ID) %>% 
  filter(str_detect(Stu_Term,"Fall...")) %>% #for ist fall only
  select(1:3)


##DATA2
FTIC_ID_from_All_fall <- STU_ENROLLMENT_SPRING20 %>% 
  filter(#Stu_StudentTypeCode == "C" & # for continuous
           #Stu_LoadIPEDSFTPT=="Full Time" & # full time
           Stu_AdmissionRecentTypeCode=="B"&  # FTIC
           Stu_DEMO_TIME_FRAME >= 201508 ) %>% # data quality better since 2015
  group_by(STU_ID) %>% 
  filter(str_detect(Stu_Term,"Fall...")) #all falls

FTIC_all_DF <- merge(FTIC_ID_from_fall,FTIC_ID_from_All_fall,by="STU_ID",all.x=TRUE)

FTIC_FALL_TERMS <- FTIC_all_DF %>% 
  group_by(STU_ID) %>% 
  arrange(Stu_DEMO_TIME_FRAME.y,.by_group=TRUE) %>% 
  select(-"Stu_DEMO_TIME_FRAME.x",-"Stu_DEMO_DATA_SOURCE.x") %>% 
  mutate(Termindex=row_number()) %>% 
  separate(Stu_AdmissionTerm, into = c("Term","Cohort"),sep=" ") 

write.csv(FTIC_FALL_TERMS,"FTIC_FALL_TERMS.csv")


#########################################
#### GET all degree  bachelor      ######
#########################################
Bachelor_deg <- STUDENT_DEGREE_SPRING20 %>% 
  filter(Deg_Desc =="Bachelor" & Deg_MajorIndicator == 1) %>%  # Majors
  select(1:3, Deg_Major,Deg_College,Deg_Department,Deg_GPAGraduation,Deg_TermGranted)

FTIC_all_falls_Bdeg <- merge(FTIC_FALL_TERMS,Bachelor_deg, by="STU_ID", all.x=TRUE) #inlcuded doulble major bachelor degree earned

###################
### clean data ####
###################
cleanFTIC_all_falls_BdegDF <- FTIC_all_falls_Bdeg %>% 
  mutate(PriorUniAPR = ifelse(Stu_TotalUniversityHours<=0,"NoPriorHrs","YesPriorHrs")) %>% 
  mutate(PriorUWFAPR = ifelse(Stu_TotalInstHours<=0,"NoUWFPriorHrs","YesUWFPriorHrs")) %>% 
  mutate(GPAEachFalls=Stu_GPAGradePoints/Stu_GPATermHours) %>% 
  mutate(APRGPA1stFall= ifelse(GPAEachFalls>= 2.77,"Above2.77","Below2.77")) %>% #UWF FTIC 1st term GPA average ==2.77
  mutate(GPAcumUWFFalls=Stu_TotalInstGradePoints/Stu_TotalInstHours) %>% 
  group_by(STU_ID) %>% 
  arrange(Termindex, .by_group=TRUE) %>% 
  select(-"Stu_GPASpecialist",-"Stu_GPASpecialistTerm",-contains("GRE"),-contains("Writing"),-contains("MATScore"),-contains("GMAT"),-contains("Doctorate"),-contains("Military"))

### create rankGPA column
cleanFTIC_all_falls_BdegDF$rankGPAEachFall <- ifelse(cleanFTIC_all_falls_BdegDF$GPAEachFalls<1.00,"under1.00", #Below average D
                                  ifelse(cleanFTIC_all_falls_BdegDF$GPAEachFalls<1.70,"under1.70", #average C-
                                         ifelse(cleanFTIC_all_falls_BdegDF$GPAEachFalls<2.00,"under2.00", #UWF APR GPA 2.0 average C
                                                ifelse(cleanFTIC_all_falls_BdegDF$GPAEachFalls<2.77,"under2.77","above2.77"))))

## rankGPA NA convert to zero
rankGPAEachFall1 <- cleanFTIC_all_falls_BdegDF$rankGPAEachFall
rankGPAEachFall1[is.na(rankGPAEachFall1)] <- 0
cleanFTIC_all_falls_BdegDF$rankGPAEachFall <- rankGPAEachFall1

GPAEachFalls1 <- cleanFTIC_all_falls_BdegDF$GPAEachFalls
GPAEachFalls1[is.na(GPAEachFalls1)] <- 0
cleanFTIC_all_falls_BdegDF$GPAEachFalls <- GPAEachFalls1

glimpse(cleanFTIC_all_falls_BdegDF)
#export  the data set
write.csv(cleanFTIC_all_falls_BdegDF,"cleanFTIC_all_falls_BdegDF.csv")

### check NA
p <- function(x){sum(is.na(x))/length(x)*100}
apply(cleanFTIC_all_falls_BdegDF, 2, p)

#comparation
FTIC_GPADEG_com <- cleanFTIC_all_falls_BdegDF %>% 
  group_by(PriorUniAPR,APRGPA1stFall) %>% 
  filter(Termindex == 1) %>% 
  summarise(Cmajors=sum(!is.na(Deg_Major)),meanPriorHrs=mean(Stu_TotalUniversityHours),CountFTIC=n())  #(meanGPA = mean(GPA1stFall),
FTIC_GPADEG_com
