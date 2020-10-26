# import enrollment data
library(readr)
STU_ENROLLMENT_SPRING20 <- read.csv("C:/Users/jsparks3/Downloads/drive-download-20200901T135512Z-001/STU_ENROLLMENT_SPRING20.csv", stringsAsFactors=TRUE)
View(STU_ENROLLMENT_SPRING20)


# choose transfer stu  using colname "admissionrecnettype" and keep university and institute hours
library(dplyr)
library(stringr)
mytransfer <- STU_ENROLLMENT_SPRING20 %>% 
  filter(str_detect(Stu_AdmissionRecentTypeDesc, "Transfer...") & Stu_DEMO_TIME_FRAME >= 200808) %>% #filter for transfer students
  group_by(STU_ID) %>% 
  arrange(Stu_DEMO_TIME_FRAME, .by_group=TRUE) %>% # arrange time low to high and within  the group 
  mutate(SemesterIndex = row_number()) %>% # this piece of code will show how many semesters the STU have
  select(1:3,Stu_AdmissionRecentTypeCode,Stu_AdmissionRecentTypeDesc,Stu_LastInstitutionDesc,
         Stu_TotalUniversityHours,Stu_TotalInstHours,SemesterIndex)  
# check max of semester index
unique(mytransfer$SemesterIndex)
#1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24

# to get theStu_TotalUniversityHours need to transfer wide format
library(data.table)
widemytransfer <- setDT(mytransfer)[, lapply(.SD, paste, collapse=" "), by= STU_ID] # collapse all values now
glimpse(widemytransfer)
widemytransfer_rm_cols <- widemytransfer[-1,]
uni.Hours.only <- str_split_fixed(widemytransfer_rm_cols$Stu_TotalUniversityHours, " ", 24) # split the values by max semester
uni.mat1 <- matrix(unlist(uni.Hours.only), ncol=24, byrow = FALSE) #create matrix
dfone <- as.data.frame(uni.mat1) # data frame
STU_ID <- widemytransfer_rm_cols$STU_ID # get the id lists

myuni.transfer.all <- cbind(STU_ID,dfone) # combine id and semesters

