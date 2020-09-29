###########################################################
#### DataShaping_APR+CRD FTIC 2013 to 2019        ######
###########################################################

deg201901 <- read_csv("C:/Users/jsparks3/Downloads/drive-download-20200901T135512Z-001/STUDENT_DEGREE_SPRING20.csv")
STU_ENROLLMENT_SPRING20 <- read.csv("C:/Users/jsparks3/Downloads/drive-download-20200901T135512Z-001/STU_ENROLLMENT_SPRING20.csv", stringsAsFactors=TRUE)
COURSE_STUDENT_INSTRUCTION_SPRING20 <- read.csv("C:/Users/jsparks3/Downloads/drive-download-20200901T135512Z-002/COURSE_STUDENT_INSTRUCTION_SPRING20.csv", stringsAsFactors=TRUE)


# FTIC
library(stringr)
#Fall FTIC only
FTICFALL <- STU_ENROLLMENT_SPRING20 %>% 
  filter(Stu_StudentTypeCode == "B" & # for FTIC
           Stu_LoadIPEDSFTPT=="Full Time" & # full time
           Stu_AdmissionRecentTypeCode=="B" & # FTIC
           Stu_DEMO_TIME_FRAME >= 201305 ) %>% # data quality since 200808
  group_by(STU_ID) %>% 
  mutate(ID_index = row_number()) %>% 
  filter(str_detect(Stu_AdmissionTerm, "Fall...")) %>% # for fall semester starters
  filter(ID_index== 1)  # index for beginner summer and came back fall are duplicated

#check Fall FTIC2017 1018
xtabs(~FTICFALL$Stu_Department+   FTICFALL$Stu_StudentTypeCode)

#Summer FTIC only
FTICSUMMER <- STU_ENROLLMENT_SPRING20 %>% 
  filter(Stu_StudentTypeCode == "B" &
           Stu_LoadIPEDSFTPT=="Full Time" & Stu_AdmissionRecentTypeCode=="B" & Stu_DEMO_TIME_FRAME >= 201305 ) %>% 
  group_by(STU_ID) %>% 
  mutate(ID_index = row_number()) %>% 
  filter(ID_index== 1) %>% 
  filter(str_detect(Stu_AdmissionTerm, "Summer...")) # summer starters
# check Summer FTIC 2017 58
xtabs(~FTICSUMMER$Stu_AdmissionTerm+   FTICSUMMER$Stu_StudentTypeCode)

# combine Fall and Sumer
FTIC_All <- rbind(FTICSUMMER,FTICFALL)
FTIC_All <- FTIC_All %>%  separate( col = Stu_AdmissionTerm, into = c("Term", "Cohort"), sep = " ")

FTIC_ALL <- FTIC_All[!duplicated(FTIC_All$STU_ID),]
xtabs(~FTIC_All$Stu_AdmissionTerm) #FTIC 2017 = 1076
#write.csv(FTIC_All,"FTIC_ALL.csv")

# create new column for Cohort
library(tidyr) # for separate and unite()
# create new column for Cohort
#FTIC_All_cohort <- separate(FTIC_All, col = Stu_AdmissionTerm, into = c("Term", "Cohort"), sep = " ")
xtabs(~FTIC_All_cohort$Cohort+FTIC_All_cohort$Stu_DepartmentCode)

FTIC_All_cohort_ID <- FTIC_All %>% select(1:4,"Cohort",contains("College"), contains("Department"))
FTIC_All_cohort_FallS <- merge(FTIC_All_cohort_ID, STU_ENROLLMENT_SPRING20, by="STU_ID", all.x=TRUE) # all terms
# creadt APR and CRD

FTIC_ALL_APR_CRD <-  FTIC_All_cohort_FallS %>% 
  filter(str_detect(Stu_Term.y, "Fall...")) %>%  #choose falls
    group_by(STU_ID) %>% 
  arrange(Stu_DEMO_TIME_FRAME.y, .by_group=TRUE) %>% 
  select(1:12,contains("College"), contains("Department"),"Stu_TotalUniversityHours","Stu_TotalInstHours","Stu_TotalInstGradePoints",
         "Stu_GPATermHours","Stu_GPAGradePoints","Stu_GPASemester","Stu_GPAUndergraduate","Stu_GPAUndergraduateTerm","Stu_CurrentTermLoad") %>% 
  mutate(NumFallTerms = row_number()) %>% 
  mutate(GPAAPRUWF = Stu_TotalInstGradePoints/Stu_TotalInstHours) %>% 
  mutate(GPAAPRind =ifelse(GPAAPRUWF>= 2.0,"GPAAPR", "GPAnonAPR")) 


unique(FTIC_ALL_APR_CRD$NumFallTerms) # 1 2 3 4 5 6 7 fall

FTIC_ALL_APR_CRD$CRDAPRUWF <-ifelse(FTIC_ALL_APR_CRD$NumFallTerms==1 & FTIC_ALL_APR_CRD$Stu_TotalUniversityHours >= 0, "FirstTerm",
                   ifelse(FTIC_ALL_APR_CRD$NumFallTerms==2 & FTIC_ALL_APR_CRD$Stu_TotalUniversityHours >= 30, "CRDAPR",
                                                                                              ifelse(FTIC_ALL_APR_CRD$NumFallTerms==3 & FTIC_ALL_APR_CRD$Stu_TotalUniversityHours>= 60, "CRDAPR",
                                                                                                     ifelse(FTIC_ALL_APR_CRD$NumFallTerms==4 & FTIC_ALL_APR_CRD$Stu_TotalUniversityHours>= 90, "CRDAPR","NonCRDAPR"))))


FTIC_ALL_APR_CRD$CRDGPAAPR <- ifelse(FTIC_ALL_APR_CRD$CRDAPRUWF=="CRDAPR" & FTIC_ALL_APR_CRD$GPAAPRind=="GPAAPR" &FTIC_ALL_APR_CRD$Stu_CurrentTermLoad>=12, "GPACRDAPR","GPACRDnonAPR")
write.csv(FTIC_ALL_APR_CRD,"FTIC_ALL_APR_CRD.csv")


B_deg <- deg201901 %>% 
  filter(Deg_MajorIndicator==1 & Deg_Desc=="Bachelor") %>% 
  select(1:4,"Deg_College","Deg_CollegeCode","Deg_Department","Deg_DepartmentCode","Deg_GPAGraduation","Deg_TermGranted")
  

### merg with deg data
 

FTIC_ALL_APR_CRD_DEG_first <- merge(FTIC_All, B_deg, by="STU_ID", all.x=TRUE ) #repested deg datasets
FTIC_ALL_APR_CRD_DEG       <- merge(FTIC_ALL_APR_CRD,B_deg, by="STU_ID", all.x=TRUE ) #repested deg datasets
# export the data
write.csv(FTIC_ALL_APR_CRD_DEG, "FTIC_ALL_APR_CRD_DEG.csv")
write.csv(FTIC_ALL_APR_CRD_DEG_first, "FTIC_ALL_APR_CRD_DEG_first.csv")









write.csv(FTIC_All_cohort,"FTIC_All_cohort.csv")
#check FTIC2017 == 1076
xtabs(~FTIC_All_cohort$Cohort)
xtabs(~FTIC_All_cohort$Cohort+FTIC_All_cohort$Stu_CollegeCode)