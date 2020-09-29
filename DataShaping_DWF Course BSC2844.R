###########################################################
#### DataShaping_DWF Course BSC2844      ######
###########################################################

deg201901 <- read_csv("C:/Users/jsparks3/Downloads/drive-download-20200901T135512Z-001/STUDENT_DEGREE_SPRING20.csv")
STU_ENROLLMENT_SPRING20 <- read.csv("C:/Users/jsparks3/Downloads/drive-download-20200901T135512Z-001/STU_ENROLLMENT_SPRING20.csv", stringsAsFactors=TRUE)
COURSE_STUDENT_INSTRUCTION_SPRING20 <- read.csv("C:/Users/jsparks3/Downloads/drive-download-20200901T135512Z-002/COURSE_STUDENT_INSTRUCTION_SPRING20.csv", stringsAsFactors=TRUE)



upCOURSE_STUDENT_INSTRUCTION_SPRING20 <- COURSE_STUDENT_INSTRUCTION_SPRING20 %>% 
  filter(str_detect(Course_CrsCombined,"BSC2844")) %>% 
  filter(Course_DEMO_TIME_FRAME>=201408) %>% 
  select(1:11,"Course_CrsGradeAwarded","Course_StuTypeCode","Course_StuMajorDesc",contains("College..."),contains("Department..."),"Course_StuRecentADMTypeDesc")

unique(upCOURSE_STUDENT_INSTRUCTION_SPRING20$Course_CrsGradeAwarded)
upCOURSE_STUDENT_INSTRUCTION_SPRING20$DWF <- ifelse(upCOURSE_STUDENT_INSTRUCTION_SPRING20$Course_CrsGradeAwarded=="A"|upCOURSE_STUDENT_INSTRUCTION_SPRING20$Course_CrsGradeAwarded=="A-"|
                                                   upCOURSE_STUDENT_INSTRUCTION_SPRING20$Course_CrsGradeAwarded=="B+"|upCOURSE_STUDENT_INSTRUCTION_SPRING20$Course_CrsGradeAwarded=="B-"|
                                                  upCOURSE_STUDENT_INSTRUCTION_SPRING20$Course_CrsGradeAwarded=="B"|upCOURSE_STUDENT_INSTRUCTION_SPRING20$Course_CrsGradeAwarded=="C"|
                                                   upCOURSE_STUDENT_INSTRUCTION_SPRING20$Course_CrsGradeAwarded=="C+"|upCOURSE_STUDENT_INSTRUCTION_SPRING20$Course_CrsGradeAwarded=="C-","Pass","Fail")




BSC2844_from2014 <- 
  
  COURSE_STUDENT_INSTRUCTION_SPRING20 %>% 
  filter()