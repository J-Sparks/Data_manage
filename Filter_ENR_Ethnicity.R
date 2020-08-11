############################
# Enrollment by enthnicity #
############################

# filter HMCSE
# stu_type - under & gradu
# under - by departments and majors
# graduate - by online or campus
# instructors by rank
# year 2015~end

STU_ENROLLMENT_SPRING20 <- 
  read_csv("C:/Users/jk99/Downloads/STU_ENROLLMENT_SPRING20.csv")

library(dplyr)
enrHMCSE_master <- STU_ENROLLMENT_SPRING20 %>% 
  filter(Stu_DEMO_TIME_FRAME>= 201508) %>% 
  select(1:4,Stu_Classification,Stu_MajorDesc,Stu_Ethnicity,
         Stu_StudentType,Stu_AdmissionRecentTypeDesc,Stu_CampusDesc,
         Stu_College,Stu_Department,Stu_OnlineCourseDelivery) %>% 
  filter(Stu_College == "Science and Engineering")
enrHMCSE_master

write.csv(enrHMCSE_master,"enrHMCSE_MASTER.csv")

# UWF
# stu_type - under & gradu
# under - by departments and majors
# graduate - by online or campus
# instructors by rank
# year 2015~end
enrUWF_master <- STU_ENROLLMENT_SPRING20 %>% 
  filter(Stu_DEMO_TIME_FRAME>= 201508) %>% 
  select(1:4,Stu_Classification,Stu_MajorDesc,Stu_Ethnicity,
         Stu_StudentType,Stu_AdmissionRecentTypeDesc,Stu_CampusDesc,
         Stu_College,Stu_Department,Stu_OnlineCourseDelivery) 
 
enrUWF_master

write.csv(enrUWF_master,"enrUWF_master.csv")

# group them by 201908 academinc year
enrUWF2019_master <- STU_ENROLLMENT_SPRING20 %>% 
  filter(Stu_DEMO_TIME_FRAME>= 201908) %>% 
  select(1:4,Stu_Classification,Stu_MajorDesc,Stu_Ethnicity,
         Stu_StudentType,Stu_AdmissionRecentTypeDesc,Stu_CampusDesc,
         Stu_College,Stu_Department,Stu_OnlineCourseDelivery,Stu_GenderCode) %>% 
  group_by(STU_ID) %>% 
  arrange(Stu_DEMO_TIME_FRAME, .by_group=TRUE) %>% 
  mutate(ID_index = row_number()) %>% 
  filter(ID_index == 1) %>% 
  mutate(AcademicYear = rep(2019))

enrUWF2019_master

# group them by 201808,201901,201905 academinc year
enrUWF2018_master <- STU_ENROLLMENT_SPRING20 %>% 
  filter(Stu_DEMO_TIME_FRAME>= 201808 & Stu_DEMO_TIME_FRAME <= 201905) %>% 
  select(1:4,Stu_Classification,Stu_MajorDesc,Stu_Ethnicity,
         Stu_StudentType,Stu_AdmissionRecentTypeDesc,Stu_CampusDesc,
         Stu_College,Stu_Department,Stu_OnlineCourseDelivery,Stu_GenderCode) %>% 
  group_by(STU_ID) %>% 
  arrange(Stu_DEMO_TIME_FRAME, .by_group=TRUE) %>% 
  mutate(ID_index = row_number()) %>% 
  filter(ID_index == 1) %>% 
  mutate(AcademicYear = rep(2018))

enrUWF2018_master

# group them by 201708,201801,201805 academinc year
enrUWF2017_master <- STU_ENROLLMENT_SPRING20 %>% 
  filter(Stu_DEMO_TIME_FRAME>= 201708 & Stu_DEMO_TIME_FRAME <= 201805) %>% 
  select(1:4,Stu_Classification,Stu_MajorDesc,Stu_Ethnicity,
         Stu_StudentType,Stu_AdmissionRecentTypeDesc,Stu_CampusDesc,
         Stu_College,Stu_Department,Stu_OnlineCourseDelivery,Stu_GenderCode) %>% 
  group_by(STU_ID) %>% 
  arrange(Stu_DEMO_TIME_FRAME, .by_group=TRUE) %>% 
  mutate(ID_index = row_number()) %>% 
  filter(ID_index == 1) %>% 
  mutate(AcademicYear = rep(2017))


enrUWF2017_master

# group them by 201608,201701,201705 academinc year
enrUWF2016_master <- STU_ENROLLMENT_SPRING20 %>% 
  filter(Stu_DEMO_TIME_FRAME>= 201608 & Stu_DEMO_TIME_FRAME <= 201705) %>% 
  select(1:4,Stu_Classification,Stu_MajorDesc,Stu_Ethnicity,
         Stu_StudentType,Stu_AdmissionRecentTypeDesc,Stu_CampusDesc,
         Stu_College,Stu_Department,Stu_OnlineCourseDelivery,Stu_GenderCode) %>% 
  group_by(STU_ID) %>% 
  arrange(Stu_DEMO_TIME_FRAME, .by_group=TRUE) %>% 
  mutate(ID_index = row_number()) %>% 
  filter(ID_index == 1) %>% 
  mutate(AcademicYear = rep(2016))


enrUWF2016_master

# group them by 201508,201601,201605 academinc year
enrUWF2015_master <- STU_ENROLLMENT_SPRING20 %>% 
  filter(Stu_DEMO_TIME_FRAME>= 201508 & Stu_DEMO_TIME_FRAME <= 201605) %>% 
  select(1:4,Stu_Classification,Stu_MajorDesc,Stu_Ethnicity,
         Stu_StudentType,Stu_AdmissionRecentTypeDesc,Stu_CampusDesc,
         Stu_College,Stu_Department,Stu_OnlineCourseDelivery,Stu_GenderCode) %>% 
  group_by(STU_ID) %>% 
  arrange(Stu_DEMO_TIME_FRAME, .by_group=TRUE) %>% 
  mutate(ID_index = row_number()) %>% 
  filter(ID_index == 1) %>% 
  mutate(AcademicYear = rep(2015))


enrUWF2015_master

### Combined all years
UWF_ENR_MASTER1 <- rbind(enrUWF2015_master,enrUWF2016_master)
UWF_ENR_MASTER2 <- rbind(UWF_ENR_MASTER1,enrUWF2017_master)
UWF_ENR_MASTER3 <- rbind(UWF_ENR_MASTER2,enrUWF2018_master)
UWF_ENR_MASTER  <- rbind(UWF_ENR_MASTER3,enrUWF2019_master)
# export data
write.csv(UWF_ENR_MASTER,"UWF_ENR_MASTER.gender.csv")

tt <- xtabs(~UWF_ENR_MASTER$Stu_Ethnicity+UWF_ENR_MASTER$Stu_DEMO_TIME_FRAME)
tt
 as.table(tt)
 write.table(tt,"tt.table")
