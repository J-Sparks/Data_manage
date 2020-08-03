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
library(readr)
metric4anon <- read_csv("metric4anon.csv")
M4 <- metric4anon
#Missing HS GPA
library(mice)
library(VIM)
MDFunct <- function(x){sum(is.na(x))/length(x)*100}
apply(metric4anon, 2, MDFunct) # Stu_HIGHSCHOOL 12.2%
#margingplot
marginplot(M4[,c("Stu_TotalUniversityHours1","GPA_HIGHSCHOOL")])

#imput is mice(data[columns], howmanyimputation defalt is 5)
impute <- mice(M4[, c("STU_ID","Stu_TotalUniversityHours1","Stu_OverallGPA1","GPA_HIGHSCHOOL")], m=3, seed = 1234 ) # if muliple cols data[,2:7]

print(impute)
impute$imp$GPA_HIGHSCHOOL

#complete data

M44 <- complete(impute, 2)

#distribution of observaed imputed values
stripplot(impute, pch=20,cex=1.2) # shows any impute points are out of distribution
# imput with replaced column ~ comparable vaiable | .imp
xyplot(impute, GPA_HIGHSCHOOL ~ Stu_OverallGPA1  | .imp, pch=20, cex=1.4) # also get along with UWF GPA
#plot 
md.pattern(M44)




ID_HSGPA <- select(M44, "STU_ID", "GPA_HIGHSCHOOL" )
UP_HSGPA <- M44$GPA_HIGHSCHOOL
metric4anon$UP_HSGPA <- UP_HSGPA
FTIC1718HMCSEDropout_DF_Completed_HSGPA <- merge(FTIC1718HMCSEDropout_DF_Completed, ID_HSGPA,
                                                 by = "STU_ID", all.x = TRUE)#contains 0 values

#FTIC1718HMCSEDropout_DF_Completed_HSGPA[is.na(FTIC1718HMCSEDropout_DF_Completed_HSGPA)] <- 0

#replace 0s to NA 
#replace(FTIC1718HMCSEDropout_DF_Completed_HSGPA$Stu_GPAHighSchool, 
 #       FTIC1718HMCSEDropout_DF_Completed_HSGPA$Stu_GPAHighSchool == 0, NA)
replace(FTIC1718HMCSEDropout_DF_Completed_HSGPA$UWFHour1stTerm,FTIC1718HMCSEDropout_DF_Completed_HSGPA$UWFHour1stTerm=="NoAttempt", 0)
replace(FTIC1718HMCSEDropout_DF_Completed_HSGPA$UWFGPA1stTerm,FTIC1718HMCSEDropout_DF_Completed_HSGPA$UWFGPA1stTerm =="NoAttempt", 0)

write.csv(FTIC1718HMCSEDropout_DF_Completed_HSGPA,"FTIC1718HMCSEDropout_DF_Completed_HSGPA.csv")


####################################################
########### filter genet crs for 2017 and 2018 FTIC HMCSE
###################################################


library(readr)
library(dplyr)
FTIC1718HMCSEDropout_DF_Completed_HSGPA <- 
  read_csv("C:/Users/jk99/Desktop/DATA 202001/FTIC1718HMCSEDropout_DF_Completed_HSGPA.csv", 
           col_types = cols(X1 = col_skip()))

# from above for filterinf fFTIC HMCSE 
FTIC201718HMCSE <- FTIC_All_cohort %>% 
  filter(Cohort== 2017 | Cohort == 2018) %>% #2017FTICHMCSE == 444
  filter(Stu_CollegeCode == "A")
# check
xtabs(~FTIC201718HMCSE$Cohort)

FTIC1718ID_only <- select(FTIC201718HMCSE,"STU_ID","Cohort")
FTIC2017ID <- filter(FTIC1718ID_only, Cohort == 2017)
FTIC2018ID <- filter(FTIC1718ID_only, Cohort == 2018)

# data set for 201708 and 201808 from crs dataset

# need import crs dataset
new_crs <- read_csv("C:/Users/jk99/Desktop/DATA 202001/COURSE_STUDENT_INSTRUCTION_SPRING20.csv") 

Fall2017crs <- new_crs %>% 
  filter(Course_Term == "Fall 2017") %>% #ist term for 2017 FTIC
  select(STU_ID,Course_Term ,Course_CrsCombined, 
         Course_CrsGradeAwarded,Course_CrsDepartmentCode) 
# Fall2018crs
Fall2018crs <- new_crs %>% 
  filter(Course_Term == "Fall 2018") %>% #ist term for 2018FTIC
  select(STU_ID,Course_Term ,Course_CrsCombined, 
         Course_CrsGradeAwarded,Course_CrsDepartmentCode) 



#filter for FTIC ID
Fall17crsHMCSE_only <- merge(FTIC2017ID, Fall2017crs, by= "STU_ID", all.x = TRUE)
Fall18crsHMCSE_only <- merge(FTIC2018ID, Fall2018crs, by= "STU_ID", all.x = TRUE)

####### merge two FTIC
###########################################################################################
Fall20172018FTICHMCSEFirtTermCrs <- rbind.data.frame(Fall17crsHMCSE_only,Fall18crsHMCSE_only)
# export the data set
tt <- xtabs(~Fall20172018FTICHMCSEFirtTermCrs$Course_CrsCombined)
tt
# descriptive data
library(tigerstats)
barchartGC(~Course_CrsCombined, data = Fall20172018FTICHMCSEFirtTermCrs)

Filterforcrs <- select(Fall20172018FTICHMCSEFirtTermCrs, 1,4:5)
##########How to spread the data with key = colname

# the most crs MAC 1147
ENC1101 <- filter(Filterforcrs, 
                  Filterforcrs$Course_CrsCombined =="ENC1101") #355
colnames(ENC1101) <-  c("STU_ID","ENC1101","ENC1101grade")

CHM2045 <- filter(Filterforcrs, 
                  Filterforcrs$Course_CrsCombined =="CHM2045") #336
colnames(CHM2045) <-  c("STU_ID","CHM2045","CHM2045grade")

BSC2010 <- filter(Filterforcrs, 
                  Filterforcrs$Course_CrsCombined =="BSC2010") #324
colnames(BSC2010) <-  c("STU_ID","BSC2010","BSC2010grade")

BSC2844 <- filter(Filterforcrs, 
                  Filterforcrs$Course_CrsCombined =="BSC2844") #173
colnames(BSC2844) <-  c("STU_ID","BSC2844","BSC2844grade")

PSY2012 <- filter(Filterforcrs, 
                  Filterforcrs$Course_CrsCombined =="PSY2012") #193
colnames(PSY2012) <-  c("STU_ID","PSY2012","PSY2012grade")

#################
## merge all crs

CrsComFticDrop <- merge(FTIC1718HMCSEDropout_DF_Completed_HSGPA, ENC1101, by="STU_ID", all.x=TRUE)
CrsComFticDrop0 <- merge(CrsComFticDrop,CHM2045,by="STU_ID", all.x= TRUE  )
CrsComFticDrop0 <- merge(CrsComFticDrop0,BSC2010,by="STU_ID", all.x= TRUE  )
CrsComFticDrop0 <- merge(CrsComFticDrop0,BSC2844,by="STU_ID", all.x= TRUE  )
CrsComFticDrop0 <- merge(CrsComFticDrop0,PSY2012,by="STU_ID", all.x= TRUE  )
# replace NA to Others
CrsComFticDrop0[is.na(CrsComFticDrop0)] = "Others"
#some duplicated data
CrsComFticDrop1 <- CrsComFticDrop0[!duplicated(CrsComFticDrop0$STU_ID), ]
summary(CrsComFticDrop1)
write.csv(CrsComFticDrop1,"CrsComFticDrop1.csv")
############
ReFTIC1718HMCSEDropout_DF_Completed_HSGPA <- 
  read_csv("C:/Users/jk99/Desktop/DATA 202001/ReFTIC1718HMCSEDropout_DF_Completed_HSGPA.csv")
keep_var <- select(CrsComFticDrop1 , 14:23)

CompletedData_crs <- cbind(ReFTIC1718HMCSEDropout_DF_Completed_HSGPA,keep_var)
write.csv(CompletedData_crs, "CompletedData_crs_08022020.csv")



######### How to paste all crs for one col by ID#########
#### other way to unite CRSs############

### using merged two FTIC see above line 230
Fall20172018FTICHMCSEFirtTermCrs <- rbind.data.frame(Fall17crsHMCSE_only,Fall18crsHMCSE_only)
Fall20172018FTICHMCSEFirtTermCrs[!duplicated(Fall20172018FTICHMCSEFirtTermCrs),]
# kept gave me  AFR repeated so took care in excell
#repload the data
UPSDFall20172018FTICHMCSEFirtTermCrs <- read.csv("C:/Users/jk99/Desktop/Dropout_crs/UPSDFall20172018FTICHMCSEFirtTermCrs.csv")
glimpse(UPSDFall20172018FTICHMCSEFirtTermCrs) #4755

library(data.table) # for set datatable function
GFall20172018FTICHMCSEFirtTermCrs <- UPSDFall20172018FTICHMCSEFirtTermCrs %>% 
  group_by(STU_ID) %>% 
  arrange(Course_CrsCombined, .by_group=TRUE ) # gave alpabetical order for CRS


SetDTFall20172018FTICHMCSEFirtTermCrs <- setDT(GFall20172018FTICHMCSEFirtTermCrs)[, lapply(.SD, paste, collapse="_") , by = STU_ID]

###################################
########### without MAT crs #######
##################################

NoMATUPSDFall20172018FTICHMCSEFirtTermCrs <- filter(UPSDFall20172018FTICHMCSEFirtTermCrs,Course_CrsDepartmentCode !="MAT" )


library(data.table) # for set datatable function
TESTDF <- NoMATUPSDFall20172018FTICHMCSEFirtTermCrs %>% 
  group_by(STU_ID) %>% 
  arrange(Course_CrsCombined, .by_group=TRUE ) # gave alpabetical order for CRS

SetDTTESTDF <- setDT(TESTDF)[, lapply(.SD, paste, collapse="_") , by = STU_ID]

colnames(SetDTTESTDF) <- c("STU_ID","Cohort","Term","FirstTermCRSs","FristTermGrades","DEPTCode")

glimpse(SetDTTESTDF)
# export this data in data Dropou crs folder
write.csv(SetDTTESTDF,"SetDTTESTDF08032020.csv")
