###########################################################
#### DataShaping_Featureselection_1st Fall ave GPA  ######
###########################################################

updated1CSEdata <- read.csv("~/Data_IR/updated1CSEdata.csv", stringsAsFactors=TRUE)
View(updated1CSEdata)
updated1CRSCSEData <- read.csv("~/Data_IR/updated1CRSCSEData.csv", stringsAsFactors=TRUE)
View(updated1CRSCSEData)

upCRSCSE <- updated1CRSCSEData
upCSE <- updated1CSEdata

#filter for 1st term courses
upCRSCSEV1 <- upCRSCSE %>% 
  group_by(UNIV_ROW_ID) %>% 
  arrange(COURSE_NAME) %>% 
  filter(DEMO_TIME_FRAME == min(DEMO_TIME_FRAME)) %>%  # choose for ist term courses
  filter(STU_SECTN_CRED >= 3 & STU_SECTN_CRED <= 5) %>%  # filter credit hours between 3 to 5 hours
  mutate(Numcrs = row_number()) #max is the max number crs taken

length(unique(upCRSCSEV1$COURSE_NAME)) #  392 courses
unique(upCRSCSEV1$Numcrs) #1 2 3 4 5 6


#make wide data DT
library(data.table)
wideupCRSCSEV1 <- setDT(upCRSCSEV1)[, lapply(.SD, paste, collapse=" ") , by = UNIV_ROW_ID]

wideupCRSCSEV2 <- wideupCRSCSEV1 %>% 
  select(1,4,6,7,8,)
###???
COURSE_NAME <- str_split_fixed(wideupCRSCSEV2$COURSE_NAME, " ", 6)#??
mat1 <- matrix(unlist(COURSE_NAME), ncol=6, byrow=TRUE)
df1 <- as.data.frame(mat1)
STU_ID <- 
STU_SECTN_CRED <- str_split_fixed(wideupCRSCSEV2$COURSE_NAME, " ", 6)
GRADE_AWARDED <- str_split_fixed(wideupCRSCSEV2$COURSE_NAME, " ", 6)



#merge with CSE data


upCSEV1 <- upCSE %>% 
  select()

