### to get an change program data 
library(readr)
library(dplyr)
library(tidyr)
### read dataset
 
STU_ENROLLMENT_SPRING21 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/DATA 202101/202101/STU_ENROLLMENT_SPRING21.csv") %>% filter(Stu_DEMO_DATA_SOURCE =="SIF") %>% filter(Stu_ClassificationCode<=4) 
STUDENT_DEGREE_SPRING21 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/DATA 202101/202101/STUDENT_DEGREE_SPRING21.csv")
table(STU_ENROLLMENT_SPRING21$Stu_ProgramCIPCode,  STU_ENROLLMENT_SPRING21$Stu_ProgramCIPDesc)
coleegedepartprogramtable <- STU_ENROLLMENT_SPRING21 %>% select(contains("college"), contains("department"), contains("program")) %>% unique()
write.csv(coleegedepartprogramtable, "UWFProgramTemp.csv")
### FTIC ID
FTIC_1stTerm <- STU_ENROLLMENT_SPRING21 %>% filter(Stu_DEMO_DATA_SOURCE =="SIF") %>% 
  filter(Stu_StudentTypeCode == "B" & # for FTIC
           Stu_LoadIPEDSFTPT=="Full Time" & # full time
           Stu_AdmissionRecentTypeCode=="B" & # FTIC
           Stu_DEMO_TIME_FRAME >= 201408 ) %>%  
  group_by(STU_ID) %>% 
   filter(stringr::str_detect(Stu_Term,"Fall...")) %>% 
  tidyr::separate(Stu_AdmissionTerm, into = c("Term","Cohort"),sep=" ") %>% 
  select(STU_ID, Term, Cohort, Stu_DEMO_TIME_FRAME,contains("college"),contains("department"), contains("program"),contains("Term") , 
         contains("type"), contains("gender"), contains("ethnicity"), -Stu_Term) %>% 
  group_by(STU_ID) %>% 
  filter(Stu_DEMO_TIME_FRAME == min(Stu_DEMO_TIME_FRAME))
#new_FTIC <- as.data.frame(apply(FTIC_1stTerm, 2, function(x)(gsub('\\s+', '',x))))

tab_CIP <- table(FTIC_1stTerm$Stu_ProgramCIPCode, FTIC_1stTerm$Stu_ProgramCIPDesc)

#2014 2015 2016 2017 2018 2019 2020 
#1385 1328 1283 1074 1091 1046 1034 
anyDuplicated(FTIC_1stTerm$STU_ID)
### add or paste begin in dataset colnmaes
colnames(FTIC_1stTerm)[4] <- paste("Begin_Term")
colnames(FTIC_1stTerm)[5] <- paste("Begin_col_code")
colnames(FTIC_1stTerm)[6] <- paste("Begin_col_desc")
colnames(FTIC_1stTerm)[7] <- paste("Begin_deprt_code")
colnames(FTIC_1stTerm)[8] <- paste("Begin_deprt_desc")
colnames(FTIC_1stTerm)[11] <- paste("Begin_CIP_code")
colnames(FTIC_1stTerm)[12] <- paste("Begin_CIP_desc")
FTIC_1stTerm[FTIC_1stTerm$Begin_deprt_desc==  "Department of Information Tech" , "Begin_deprt_desc"] <- "Information Technology"
UNA<- FTIC_1stTerm[FTIC_1stTerm$Begin_col_desc=="No College Designated",]
FTIC_1stTerm[FTIC_1stTerm$Begin_col_desc==  "No College Designated" , "Begin_deprt_desc"] <- "Undeclared"

table(FTIC_1stTerm$Begin_deprt_desc,  FTIC_1stTerm$Begin_deprt_code)
glimpse(FTIC_1stTerm) 
unique(FTIC_1stTerm$Begin_deprt_desc)
### additional infor

add_terms <- STU_ENROLLMENT_SPRING21 %>% 
  filter(Stu_DEMO_DATA_SOURCE =="SIF") %>% 
  filter(Stu_DEMO_TIME_FRAME >= 201408) %>% 
  filter(Stu_ClassificationCode<=4) %>% 
  select(STU_ID, Stu_DEMO_TIME_FRAME, Stu_Term,contains("college"), contains("department"), contains("ProgramCIP") )

FTIC_ID_ENC <- merge(FTIC_1stTerm, add_terms, by="STU_ID", all.x = T ) %>% 
  filter( (Cohort == 2014 & Stu_DEMO_TIME_FRAME>=201408) |
                (Cohort == 2015 & Stu_DEMO_TIME_FRAME>=201508 ) |
                        (Cohort == 2016 & Stu_DEMO_TIME_FRAME>=201608) |
                               (Cohort == 2017 & Stu_DEMO_TIME_FRAME>=201708 )|
                                      (Cohort == 2018 & Stu_DEMO_TIME_FRAME>=201808) |
                                             (Cohort == 2019 & Stu_DEMO_TIME_FRAME>=201908 )|
                                                    (Cohort == 2020 & Stu_DEMO_TIME_FRAME>=202008 ))
                                                          

table(FTIC_ID_ENC$Cohort, FTIC_ID_ENC$Stu_DEMO_TIME_FRAME)

### add deg
 deg_info <- STUDENT_DEGREE_SPRING21 %>% 
   select(1,2,4,5,contains("CIP"), contains("college"), contains("department"), contains("program"), Deg_TermGranted, Deg_Desc) %>% 
   filter(Deg_Desc=="Bachelor")   
   #arrange(Deg_DEMO_TIME_FRAME) %>% top_n(-1, Deg_DEMO_TIME_FRAME)
 deg_info1 <-  deg_info[!duplicated(deg_info$STU_ID),]
 anyDuplicated(deg_info1$STU_ID)
 
### add deg info
 
 FTIC_ID_ENC_deg <- merge(FTIC_ID_ENC, deg_info1, by="STU_ID" , all.x = T)
 
  
### CODE TIME
 
 FTIC_CP_data <- FTIC_ID_ENC_deg %>% 
   group_by(STU_ID) %>% select(-Term) %>% 
   arrange(Stu_DEMO_TIME_FRAME) %>% 
   mutate(Gradu_Time=ifelse((Cohort==2014 & Deg_TermGranted <= 201805), "Gradu<=4",
                            ifelse((Cohort==2014 & (Deg_TermGranted > 201805 & Deg_TermGranted <= 201905)),"Gradu=5",
                                   ifelse((Cohort==2014 & (Deg_TermGranted > 201905 & Deg_TermGranted <= 202005)),'Gradu=6',
                                          ifelse((Cohort==2014 & (Deg_TermGranted > 202005 & Deg_TermGranted <= 202008)),'Gradu=7',    
                                                 ifelse((Cohort==2015 & Deg_TermGranted <= 201905), "Gradu<=4",
                                                        ifelse((Cohort==2015 & (Deg_TermGranted > 201905 & Deg_TermGranted <= 202005)),"Gradu=5",
                                                               ifelse((Cohort==2015 & (Deg_TermGranted > 202005 & Deg_TermGranted <= 202105)),'Gradu=6',
                                                                      ifelse((Cohort==2016 & Deg_TermGranted <= 202005), "Gradu<=4",
                                                                             ifelse((Cohort==2016 & (Deg_TermGranted > 202005 & Deg_TermGranted <= 202105)),"Gradu=5",
                                                                                    ifelse((Cohort==2016 & (Deg_TermGranted > 202105 & Deg_TermGranted <= 202205)),'Gradu=6',"Others")))))))))))  %>% group_by(STU_ID) %>% arrange(Stu_DEMO_TIME_FRAME) %>% 
   group_by(STU_ID) %>% arrange(Stu_DEMO_TIME_FRAME) %>% 
   mutate(ChangedProg=ifelse(Stu_ProgramCIPCode == lag(Stu_ProgramCIPCode, default = Stu_ProgramCIPCode[1]), 0,1)) %>% 
   mutate(ChangedDeprt=ifelse(Stu_DepartmentCode == lag(Stu_DepartmentCode, default = Stu_DepartmentCode[1]), 0,1)) %>% 
   mutate(ChangedCol=ifelse(Stu_College == lag(Stu_College, default = Stu_College[1]), 0,1))  %>% 
   mutate(On_Time = ifelse(Gradu_Time == "Gradu<=4", "Yes", "No")) %>% 
   mutate(CumChangedProg = cumsum(ChangedProg),
          CumChangedDeprt = cumsum(ChangedDeprt),
          CumChangedCol = cumsum(ChangedCol)) %>%
   mutate( MaxCPTimes =  max(CumChangedProg),
           MaxCDTimes= max(CumChangedDeprt),
           MaxCCTimes= max(CumChangedCol)) %>%  
   mutate(ChangeInd=ifelse(MaxCPTimes==0,"StayedProg", ifelse( (MaxCCTimes==0 & MaxCDTimes==0 & MaxCPTimes>0), "ChangedProg",ifelse((MaxCCTimes==0 & MaxCDTimes>0 ), "ChangedDepart", ifelse( (MaxCCTimes>0), "ChangedCol","Others"))))) %>% 
   tidyr::separate(Stu_Term, c("Term","Year")) %>% 
   group_by(STU_ID,Term,Year) %>% arrange(Term) %>% group_by(STU_ID, Term)  %>%  mutate(TermInd = row_number()) %>% 
   mutate(TermYear=paste( TermInd,Term, sep = "Year" )) %>% 
    filter(Cohort<= 2018)
 
 ex2 <- FTIC_CP_data[FTIC_CP_data$STU_ID== "7E4B110FD728E884B9389D8BBC408541A5CDD649",]
table(FTIC_CP_data$Begin_deprt_code, FTIC_CP_data$Begin_deprt_desc) 
 
 write.csv(FTIC_CP_data, "V2_ID_ENC_DEG_DATA.csv")
 
 
