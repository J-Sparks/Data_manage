###########################################################
#### DataShaping_Featureselection_1st Fall ave GPA  ######
###########################################################

updated1CSEdata <- read.csv("~/Data_IR/updated1CSEdata.csv", stringsAsFactors=TRUE)
View(updated1CSEdata)
updated1CRSCSEData <- read.csv("~/Data_IR/updated1CRSCSEData.csv", stringsAsFactors=TRUE)
View(updated1CRSCSEData)

upCRSCSE <- updated1CRSCSEData
upCSE <- updated1CSEdata

## detect summer starter term
upCRSCSE05 <- upCRSCSE$DEMO_TIME_FRAME
as.factor(upCRSCSE05)
s = str_sub(upCRSCSE05,5) # left to right 5th...
upCRSCSE$DEMO_TIME_FRAME <- as.numeric(s) 
glimpse(upCRSCSE)

#filter for 1st term courses
upCRSCSEV1 <- upCRSCSE %>% 
  group_by(UNIV_ROW_ID) %>% 
  arrange(COURSE_NAME) %>% 
  filter(DEMO_TIME_FRAME == min(DEMO_TIME_FRAME)) %>%  # choose for Fall term ==8 term courses
  filter(STU_SECTN_CRED >= 3 & STU_SECTN_CRED <= 5) %>%  # filter credit hours between 3 to 5 hours
  mutate(Numcrs = row_number()) #max is the max number crs taken

length(unique(upCRSCSEV1$COURSE_NAME)) #  392 courses
unique(upCRSCSEV1$Numcrs) #1 2 3 4 5 6 the max course are 6


#make wide data DT
library(data.table)
wideupCRSCSEV1 <- setDT(upCRSCSEV1)[, lapply(.SD, paste, collapse=" ") , by = UNIV_ROW_ID]


wideupCRSCSEV2 <- wideupCRSCSEV1 %>% 
  select(1,4,6,7,8,) 

wideupCRSCSEV2_1 <- wideupCRSCSEV2[-1,] #remove colnmaes  
###???
COURSE_NAME <- str_split_fixed(wideupCRSCSEV2_1$COURSE_NAME, " ", 6) #max course 6
mat1 <- matrix(unlist(COURSE_NAME), ncol=6, byrow=FALSE)
df1 <- as.data.frame(mat1)
UNIV_ROW_ID <- wideupCRSCSEV2_1$UNIV_ROW_ID 

STU_SECTN_CRED <- str_split_fixed(wideupCRSCSEV2_1$STU_SECTN_CRED, " ", 6)
mat2 <- matrix(unlist(STU_SECTN_CRED), ncol=6, byrow=FALSE)
df2 <- as.data.frame(mat2)

GRADE_AWARDED <- str_split_fixed(wideupCRSCSEV2_1$GRADE_AWARDED, " ", 6)
mat3 <- matrix(unlist(GRADE_AWARDED), ncol=6, byrow=FALSE)
df3 <- as.data.frame(mat3)


#merge with CSE data
merge1 <- cbind(UNIV_ROW_ID,df1)
merge2 <- cbind(merge1,df2)
merge3 <- cbind(merge2,df3)
colnames(merge3) <- c("UNIV_ROW_ID","CRSNAME1","CRSNAME2","CRSNAME3","CRSNAME4","CRSNAME5","CRSNAME6",
                      "Credit1","Credit2","Credit3","Credit4","Credit5","Credit6",
                      "CRScount1","CRScount2","CRScount3","CRScount4","CRScount5","CRScount6")




CSE_wideCRS  <- merge(merge3, CSEdata, by="UNIV_ROW_ID", all.y=TRUE)
glimpse(merge3)#""


write.csv(CSE_wideCRS,"wide_AllCSECRS_data.csv")
