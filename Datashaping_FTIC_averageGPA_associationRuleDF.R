library(readr)
metric4anon <- read_csv("C:/Users/jsparks3/Downloads/metric4anon.csv")

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
         contains("Total"),Stu_CurrentTermLoad,Stu_APInd,contains("ACT"),contains("SAT"),Stu_ResidenceHall) %>% 
  mutate(GPA1stFall=Stu_GPAGradePoints/Stu_GPATermHours)

#Nas to zero values
GPA1stFall_na <- FTIC_1stTerm_ALL_select$GPA1stFall
GPA1stFall_na[is.na(GPA1stFall_na)] <- 0
FTIC_1stTerm_ALL_select$GPA1stFall <- GPA1stFall_na # 2.772962 UWF 1st fall average B- ==2.7
attach(FTIC_1stTerm_ALL_select)
#UWF 1st term GPA Average
FTIC_1stTerm_ALL_select$averageGPA1stFall <- ifelse(GPA1stFall>= 2.7,"Above2.7","Below2.7")
#UWF 1st term GPA 5 rank
FTIC_1stTerm_ALL_select$rankGPA1stFall <- ifelse(FTIC_1stTerm_ALL_select$GPA1stFall<1.0,"Under1.0",
                                                 ifelse(FTIC_1stTerm_ALL_select$GPA1stFall<1.5,"under1.5",
                                                        ifelse(FTIC_1stTerm_ALL_select$GPA1stFall<2.0,"under2.0",
                                                               ifelse(FTIC_1stTerm_ALL_select$GPA1stFall<2.3,"under2.3","above2.3"))))
# to get better GPA high school from metric4
HsGPA <- metric4anon %>% 
  select(STU_ID, GPA_HIGHSCHOOL)

#combine with hs gpa
newFTIC_1stTerm_GPA <- merge(FTIC_1stTerm_ALL_select,HsGPA, by="STU_ID", all.x = TRUE);glimpse(newFTIC_1stTerm_GPA)

#check NA
p <- function(x){sum(is.na(x))/length(x)*100}
apply(newFTIC_1stTerm_GPA, 2, p)#GPA_HIGHSCHOOL 38.95080 





############################################
#### select variables  #####################
###########################################

GPAFTIC_1st_term_association <- newFTIC_1stTerm_GPA %>% 
  filter(Stu_Age <= 19) %>% #mean age == 18.55
  select(Gender="Stu_Gender",Count="Stu_County",Ethnicity="Stu_Ethnicity",college="Stu_College",Stu_Department,
         ResidenceHall="Stu_ResidenceHall",Stu_DivisionCode,Stu_MajorDesc,averageGPA1stFall)


xtabs(~GPAFTIC_1st_term_association$averageGPA1stFall)
apply(GPAFTIC_1st_term_association, 2, p)
write.csv(GPAFTIC_1st_term_association,"GPAFTIC_1st_term_association.csv")
