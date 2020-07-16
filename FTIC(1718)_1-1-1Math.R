# import data
new_enr <- read_csv("C:/Users/jk99/Desktop/DATA 202001/STU_ENROLLMENT_SPRING20.csv") 

library(stringr)
#Fall FTIC only
FTIC_1stTerm_enr_Fall <- new_enr %>% 
  filter(Stu_StudentTypeCode == "B" & # for FTIC
           Stu_LoadIPEDSFTPT=="Full Time" & # full time
           Stu_AdmissionRecentTypeCode=="B" & # FTIC
           Stu_DEMO_TIME_FRAME >= 200805 ) %>% # data quality since 200808
  group_by(STU_ID) %>% 
  mutate(ID_index = row_number()) %>% 
  filter(ID_index== 1) %>%  # index for beginner summer and came back fall are duplicated
  filter(str_detect(Stu_AdmissionTerm, "Fall...")) # for fall semester starters
#check Fall FTIC2017 1018
xtabs(~FTIC_1stTerm_enr_Fall$Stu_AdmissionTerm+ FTIC_1stTerm_enr_Fall$Stu_StudentTypeCode)

#Summer FTIC only
FTIC_1stTerm_enr_Summer <- new_enr %>% 
  filter(Stu_StudentTypeCode == "B" &
           Stu_LoadIPEDSFTPT=="Full Time" & Stu_AdmissionRecentTypeCode=="B" & Stu_DEMO_TIME_FRAME >= 200805 ) %>% 
  group_by(STU_ID) %>% 
  mutate(ID_index = row_number()) %>% 
  filter(ID_index== 1) %>% 
  filter(str_detect(Stu_AdmissionTerm, "Summer...")) # summer starters
# check Summer FTIC 2017 58
xtabs(~FTIC_1stTerm_enr_Summer$Stu_AdmissionTerm+   FTIC_1stTerm_enr_Summer$Stu_StudentTypeCode)

# combine Fall and Sumer
FTIC_All <- rbind(FTIC_1stTerm_enr_Summer,FTIC_1stTerm_enr_Fall)
xtabs(~FTIC_All$Stu_AdmissionTerm) #FTIC 2017 = 1076



library(tidyr) # for separate and unit()
# create new column for Cohort
FTIC_All_cohort <- separate(FTIC_All, col = Stu_AdmissionTerm, into = c("Term", "Cohort"), sep = " ")
#write.csv(FTIC_All_cohort,"FTIC_All_cohort.csv")
#check FTIC2017 == 1076
xtabs(~FTIC_All_cohort$Cohort)
xtabs(~FTIC_All_cohort$Cohort+FTIC_All_cohort$Stu_CollegeCode)

# Filter for 2017 FTIC HMCSE for 1-1-1 analysis

FTIC2017HMCSE <- FTIC_All_cohort %>% 
  filter(Cohort== 2017) %>% #2017FTICHMCSE == 444
  filter(Stu_CollegeCode == "A") %>% 
  select(STU_ID, Stu_MajorDesc,Stu_GenderCode, Stu_Ethnicity,Stu_CountyCode,
         Stu_GPAHighSchool,Stu_Department,
         Stu_TotalUniversityHours) # high school GPA are not in this dataset
colnames(FTIC2017HMCSE)[colnames(FTIC2017HMCSE) == "Stu_TotalUniversityHours"] <- "Prior_Hrs"

# need import crs dataset
new_crs <- read_csv("C:/Users/jk99/Desktop/DATA 202001/COURSE_STUDENT_INSTRUCTION_SPRING20.csv") 

Fall2017MAT <- new_crs %>% 
  filter(Course_Term == "Fall 2017") %>% #ist term for 2017 FTIC
  filter(Course_CrsDepartmentCode == "MAT") %>%
  filter(Course_CrsPrefix=="MAC") %>% # MAT == Math department MAC course only not SAT
  select(STU_ID,Course_Term ,Course_CrsTitle, Course_CrsCombined, Course_CrsPrefix,
         Course_CrsNumber,Course_CrsSuffix,Course_CrsGradeAwarded,Course_CrsDepartmentCode) 

#Merge with 2018FTICHMCSE
ID_FTIC2017HMCSE_1termMATH <- merge(FTIC2017HMCSE,Fall2017MAT, by="STU_ID", all.x = TRUE ) %>% 
  group_by(STU_ID) %>% 
  arrange(Course_CrsNumber, .by_group=TRUE) %>% 
  mutate(ID_index = row_number()) # make sure all have number 1
summary(ID_FTIC2017HMCSE_1termMATH) # 444
#write.csv(FTIC2018HMCSE_1termMATH,"FTIC2017HMCSE_1termMATH.csv")

# need to get for DROP out index

# filter FTIC2018 ID with all terms
# get all terms from enr data by ID
ID <- select(ID_FTIC2017HMCSE_1termMATH, "STU_ID")

# terms 201808,201901,201905,201908 (201908 will use for dropout)
UWFGPA_df <-  new_enr %>% 
  select(STU_ID,Stu_DEMO_TIME_FRAME,Stu_TotalInstGradePoints,Stu_TotalInstHours,
         Stu_GPATermHours,Stu_GPAGradePoints) %>% 
  mutate(UWFGPA = Stu_TotalInstGradePoints/Stu_TotalInstHours)  %>% 
  filter(Stu_DEMO_TIME_FRAME>=201708 & Stu_DEMO_TIME_FRAME <= 201808)

# only FTIC GPA
ID_GPA <- merge(ID, UWFGPA_df, by="STU_ID", all.x=TRUE ) %>% 
  select(STU_ID,Stu_DEMO_TIME_FRAME, UWFGPA) %>% 
  group_by(STU_ID) %>% 
  arrange(Stu_DEMO_TIME_FRAME, .by_group=TRUE) %>% 
  filter(Stu_DEMO_TIME_FRAME == max(Stu_DEMO_TIME_FRAME)) %>% # keep the last term with GPA
  mutate(Drop1Year=ifelse(Stu_DEMO_TIME_FRAME==201808,"Retained","Dropped")) # creat Ddrop1Yesr factor

# NA to zero value
ID_GPA[is.na(ID_GPA)] <-  0

# Combine two ID dataset GPA and MAth

FTIC2017HMCSEDropout_DF <- cbind(ID_GPA,ID_FTIC2017HMCSE_1termMATH )
FTIC2017HMCSEDropout_DF$STU_ID <- FTIC2017HMCSEDropout_DF$STU_ID...1
# To get an ist term hours and grade point
Term201708GPAPoint <- UWFGPA_df %>% 
  filter(Stu_DEMO_TIME_FRAME == 201708) %>% # ist term for FTIC2018
  select(STU_ID,Stu_GPATermHours,Stu_GPAGradePoints) %>%  # for ist term GPA
  mutate(UWFGPA1Term = Stu_GPAGradePoints/Stu_GPATermHours) # create UWFGPA1term

# add a new column with FTIC2018HMCSEDropout_DF
FTIC2017HMCSEDropout_DF <- merge(FTIC2017HMCSEDropout_DF, Term201708GPAPoint, by="STU_ID", all.x = TRUE)

# Creaning data for the modeling
glimpse(FTIC2017HMCSEDropout_DF)

FTIC2017HMCSEDropout_DF_completed <- FTIC2017HMCSEDropout_DF %>% 
  select(STU_ID,UWFGPA,Prior_Hrs,Stu_MajorDesc,Stu_GenderCode,Stu_Ethnicity,
         Stu_CountyCode,Course_CrsCombined,Course_CrsGradeAwarded,
         Stu_GPATermHours,UWFGPA1Term,Drop1Year);glimpse(FTIC2017HMCSEDropout_DF_completed)
# replace NA for Noattempt
FTIC2017HMCSEDropout_DF_completed[is.na(FTIC2017HMCSEDropout_DF_completed)] <-  "NoAttempt"

xtabs(~FTIC2017HMCSEDropout_DF_completed$Drop1Year)#93/351

# change colnames
names(FTIC2017HMCSEDropout_DF_completed) <- c("STU_ID","UWFGPA1stYear","Prior_Hours","StartMajor",
                                              "Gender","Ethnicity","County","FirstMathCrs","FirstMathCrsGrade",
                                              "UWFHour1stTerm","UWFGPA1stTerm","Drop1stYear");glimpse(FTIC2017HMCSEDropout_DF_completed)

#Save the final dataset
write.csv(FTIC2017HMCSEDropout_DF_completed,"FTIC2017HMCSEDropout_DF_completed.csv")
write.csv(FTIC2018HMCSEDropout_DF_completed,"FTIC2018HMCSEDropout_DF_completed.csv")

# FTIC 2017 and 18 combined
FTIC1718HMCSEDropout_DF_Completed <- rbind(FTIC2017HMCSEDropout_DF_completed,FTIC2018HMCSEDropout_DF_completed)
#write.csv(FTIC1718HMCSEDropout_DF_Completed,"FTIC1718HMCSEDropout_DF_Completed.csv")

#now you can clean data using excel

# to get an HS GPA
#import metric4 data set
# to get an HS GPA
#import metric4 data set
library(readr)
metric4anon <- read_csv("metric4anon.csv")
M4 <- metric4anon
#Missing HS GPA
library(mice)
library(VIM)
MDFunct <- function(x){sum(is.na(x))/length(x)*100}
apply(metric4anon, 2, MDFunct) # GPA_HIGHSCHOOL 12.2%
#margingplot
marginplot(M4[,c("Stu_TotalUniversityHours1","GPA_HIGHSCHOOL")])

#imput is mice(data[columns], howmanyimputation defalt is 5)
impute <- mice(M4[, c("STU_ID","Stu_TotalUniversityHours1","Stu_OverallGPA1","GPA_HIGHSCHOOL")], m=3, seed = 1234 ) # if muliple cols 

print(impute)
impute$imp$GPA_HIGHSCHOOL

#complete data

M44 <- complete(impute, 2) # choose 2nd impute

#distribution of observaed imputed values
stripplot(impute, pch=20,cex=1.2) # shows any impute points are out of distribution
# imput with replaced column ~ comparable vaiable | .imp
xyplot(impute, GPA_HIGHSCHOOL ~ Stu_OverallGPA1  | .imp, pch=20, cex=1.4) # also get along with UWF GPA
#plot 
md.pattern(M44)




ID_HSGPA <- select(M44, "STU_ID", "GPA_HIGHSCHOOL" )
FTIC1718HMCSEDropout_DF_Completed_HSGPA <- merge(FTIC1718HMCSEDropout_DF_Completed, ID_HSGPA,
                                                 by = "STU_ID", all.x = TRUE)#contains 0 values

#FTIC1718HMCSEDropout_DF_Completed_HSGPA[is.na(FTIC1718HMCSEDropout_DF_Completed_HSGPA)] <- 0

#replace 0s to NA 
#replace(FTIC1718HMCSEDropout_DF_Completed_HSGPA$GPA_HIGHSCHOOL, 
 #       FTIC1718HMCSEDropout_DF_Completed_HSGPA$GPA_HIGHSCHOOL == 0, NA)

replace(FTIC1718HMCSEDropout_DF_Completed_HSGPA$UWFHour1stTerm,FTIC1718HMCSEDropout_DF_Completed_HSGPA$UWFHour1stTerm=="NoAttempt", 0)
replace(FTIC1718HMCSEDropout_DF_Completed_HSGPA$UWFGPA1stTerm,FTIC1718HMCSEDropout_DF_Completed_HSGPA$UWFGPA1stTerm =="NoAttempt", 0)

write.csv(FTIC1718HMCSEDropout_DF_Completed_HSGPA,"FTIC1718HMCSEDropout_DF_Completed_HSGPA.csv")
