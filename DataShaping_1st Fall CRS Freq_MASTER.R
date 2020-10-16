###########################################################
#### DataShaping_FREQUENCY COURSE NAME 1st Fall ave GPA  ######
###########################################################

# before imported check NAs (cols: first Fall and second Fall) with withdrawn FTICs, should be zero values for GPA

updated1CSEdata <- read.csv("~/Data_IR/CSEdata.csv", stringsAsFactors=TRUE)
View(updated1CSEdata)

updated1CRSCSEData <- read.csv("~/Data_IR/CRSCSEData.csv", stringsAsFactors=TRUE)
View(updated1CRSCSEData)

upCRSCSE <- updated1CRSCSEData
upCSE <- updated1CSEdata

#### updated for FTIC 2020
CSE2020 <- upCSE %>% 
  filter(COHORT_YEAR==20202021)

CSE2020rest <- upCSE %>% 
  filter(COHORT_YEAR<20202021)


up2020CSEdata <- read.csv("~/Data_IR/up2020CSEdata.csv", stringsAsFactors=TRUE)
upCSE2020 <- up2020CSEdata  %>% select(1, "HOURS_BROUGHT_TO_UNIVERSITY","AP_CREDITS","ETHNICITY")
lowerCSE2020 <- CSE2020 %>%  select("UNIV_ROW_ID")
upupCSE2020 <- merge(upCSE2020, lowerCSE2020, by="UNIV_ROW_ID", all.y=TRUE);glimpse(upupCSE2020)
CSE2020$HOURS_BROUGHT_TO_UNIVERSITY <- upupCSE2020$HOURS_BROUGHT_TO_UNIVERSITY
CSE2020$AP_CREDITS <- upupCSE2020$AP_CREDITS
CSE2020$ETHNICITY <- upupCSE2020$ETHNICITY
glimpse(CSE2020)
upCSE <- rbind(CSE2020rest,CSE2020)
upCSE$COUNTY_GROUP[upCSE$COUNTY_GROUP == "Tri County"] <- "Tri-County" 
upCSE$COUNTY_GROUP[upCSE$COUNTY_GROUP == "Non-Florida/Alabama Counties"] <- "Non-Florida/Alabama" 
glimpse(upCSE)


detach(package:plyr)
library(dplyr)

##########################################################################
### arrange freq courses in r ############################################
#########################################################################

upCRSEFall <- upCRSCSE %>% 
  group_by(UNIV_ROW_ID) %>% 
  filter(DEMO_TIME_FRAME == 201208 | DEMO_TIME_FRAME == 201308 | DEMO_TIME_FRAME == 201408 | 
           DEMO_TIME_FRAME == 201508 | DEMO_TIME_FRAME == 201608 | DEMO_TIME_FRAME == 201708 | 
           DEMO_TIME_FRAME == 201808 | DEMO_TIME_FRAME == 201908 | DEMO_TIME_FRAME == 202008) %>%  # choose for Fall term 
  filter(DEMO_TIME_FRAME == min(DEMO_TIME_FRAME), .by_group=TRUE) %>% 
  mutate(Numcrs = row_number())


length(unique(upCRSEFall$COURSE_NAME)) #  512 courses
unique(upCRSEFall$Numcrs) #1 2 3 4 5 6 7 8 9 the max course are 9

#########################################
######## group by freq. ################
####################################


upCRSEFallDT <- upCRSEFall %>% 
  group_by(COURSE_NAME) %>%
  summarise(Freq=n()) %>% 
  ungroup() %>% 
  arrange(-Freq)
freqtable <- as.data.frame(upCRSEFallDT)

Frecrsname <- merge(upCRSEFall, freqtable, by="COURSE_NAME", all.y=TRUE)
glimpse(Frecrsname)

arraFreupCRSFall <- Frecrsname %>% 
group_by(UNIV_ROW_ID,COURSE_NAME) %>% 
  arrange(-Freq)

#m ake wide data DT
library(data.table)
wideupCRSCSEV1 <- setDT(arraFreupCRSFall)[, lapply(.SD, paste, collapse=" ") , by = UNIV_ROW_ID]
glimpse(wideupCRSCSEV1)

wideupCRSCSEV2 <- wideupCRSCSEV1 %>% 
  select(1,2,6,7) 
glimpse(wideupCRSCSEV2)



wideupCRSCSEV2_1 <- wideupCRSCSEV2[-1,] #remove colnmaes  
library(stringr)
COURSE_NAME <- str_split_fixed(wideupCRSCSEV2_1$COURSE_NAME, " ", 9) #max course 9
mat1 <- matrix(unlist(COURSE_NAME), ncol=9, byrow=FALSE)
df1 <- as.data.frame(mat1)
UNIV_ROW_ID <- wideupCRSCSEV2_1$UNIV_ROW_ID 

STU_SECTN_CRED <- str_split_fixed(wideupCRSCSEV2_1$STU_SECTN_CRED, " ", 9)
mat2 <- matrix(unlist(STU_SECTN_CRED), ncol=9, byrow=FALSE)
df2 <- as.data.frame(mat2)

GRADE_AWARDED <- str_split_fixed(wideupCRSCSEV2_1$GRADE_AWARDED, " ", 9)
mat3 <- matrix(unlist(GRADE_AWARDED), ncol=9, byrow=FALSE)
df3 <- as.data.frame(mat3)


#merge with CSE data
merge1 <- cbind(UNIV_ROW_ID, df1)
merge2 <- cbind(merge1,df2)
merge3 <- cbind(merge2,df3)
colnames(merge3) <- c("UNIV_ROW_ID","CRSNAME1","CRSNAME2","CRSNAME3","CRSNAME4","CRSNAME5","CRSNAME6","CRSNAME7","CRSNAME8","CRSNAME9",
                      "Credit1","Credit2","Credit3","Credit4","Credit5","Credit6","Credit7","Credit8","Credit9",
                      "CRSgrade1","CRSgrade2","CRSgrade3","CRSgrade4","CRSgrade5","CRSgrade6","CRSgrade7","CRSgrade8","CRSgrade9")

str(merge3)

# merge with CSE data
upCSE <- upCSE %>% 
  mutate(aveGPAInd=ifelse(FIRST_FALL_GPA >=2.77,"AboveGPA","BelowGPA" )) 

CSE_wideCRS  <- merge(merge3, upCSE, by="UNIV_ROW_ID", all.y=TRUE)
glimpse(CSE_wideCRS)

pMISS <- function(x){sum(is.na(x))/length(x)*100}
apply(CSE_wideCRS, 2, pMISS)

write.csv(CSE_wideCRS,"V1FreqOrderwith9crs_MASTER_CRSCSE_allcourse_tier.csv")


###############################################################
############ Alphabetical order ##################################
#################################################################
#filter for 1st term courses
upCRSCSEV1 <- upCRSCSE %>% 
  group_by(UNIV_ROW_ID) %>% 
  arrange(COURSE_NAME) %>% 
  filter(DEMO_TIME_FRAME == 201208 | DEMO_TIME_FRAME == 201308 | DEMO_TIME_FRAME == 201408 | 
           DEMO_TIME_FRAME == 201508 | DEMO_TIME_FRAME == 201608 | DEMO_TIME_FRAME == 201708 | 
           DEMO_TIME_FRAME == 201808 | DEMO_TIME_FRAME == 201908 | DEMO_TIME_FRAME == 202008 ) %>%  # choose for Fall term 
  filter(DEMO_TIME_FRAME == min(DEMO_TIME_FRAME), .by_group=TRUE) %>% 
  #filter(STU_SECTN_CRED >= 3 & STU_SECTN_CRED <= 4) %>%  # filter credit hours between 3 to 4 hours,PHY2048C(5hrs omitted one term offered)
  mutate(Numcrs = row_number()) #max is the max number crs taken

length(unique(upCRSCSEV1$COURSE_NAME)) #  388 courses/ 466
unique(upCRSCSEV1$Numcrs) #1 2 3 4 5 6 the max course are 6 /1 2 3 4 5 6 7 8 9


#make wide data DT
library(data.table)
wideupCRSCSEV1 <- setDT(upCRSCSEV1)[, lapply(.SD, paste, collapse=" ") , by = UNIV_ROW_ID]


wideupCRSCSEV2 <- wideupCRSCSEV1 %>% 
  select(1,4,6,7,8,) 

wideupCRSCSEV2_1 <- wideupCRSCSEV2[-1,] #remove colnmaes  
###???
COURSE_NAME <- str_split_fixed(wideupCRSCSEV2_1$COURSE_NAME, " ", 9) #max course 9
mat1 <- matrix(unlist(COURSE_NAME), ncol=9, byrow=FALSE)
df1 <- as.data.frame(mat1)
UNIV_ROW_ID <- wideupCRSCSEV2_1$UNIV_ROW_ID 

STU_SECTN_CRED <- str_split_fixed(wideupCRSCSEV2_1$STU_SECTN_CRED, " ", 9)
mat2 <- matrix(unlist(STU_SECTN_CRED), ncol=9, byrow=FALSE)
df2 <- as.data.frame(mat2)

GRADE_AWARDED <- str_split_fixed(wideupCRSCSEV2_1$GRADE_AWARDED, " ", 9)
mat3 <- matrix(unlist(GRADE_AWARDED), ncol=9, byrow=FALSE)
df3 <- as.data.frame(mat3)


#merge with CSE data
merge1 <- cbind(UNIV_ROW_ID,df1)
merge2 <- cbind(merge1,df2)
merge3 <- cbind(merge2,df3)
colnames(merge3) <- c("UNIV_ROW_ID","CRSNAME1","CRSNAME2","CRSNAME3","CRSNAME4","CRSNAME5","CRSNAME6","CRSNAME7","CRSNAME8","CRSNAME9",
                      "Credit1","Credit2","Credit3","Credit4","Credit5","Credit6","Credit7","Credit8","Credit9",
                      "CRSgrade1","CRSgrade2","CRSgrade3","CRSgrade4","CRSgrade5","CRSgrade6","CRSgrade7","CRSgrade8","CRSgrade9")
### NA in merge3
# replace crsname NSs to "NO"

str(merge3)
CSE_wideCRS  <- merge(merge3, upCSE, by="UNIV_ROW_ID", all.y=TRUE)
glimpse(CSE_wideCRS)

write.csv(CSE_wideCRS,"AlphOrderwith9crs_MASTER_CRSCSE_allcourse_tier.csv")



########### DESCRIPTIVE #########################
dfCRS <- data.table( crsname = upCRSEFall$COURSE_NAME)
dfCRS[,.(count = .N), by = crsname][, percent := prop.table(count)*100][order(-count),][]


upCRSEFall <- upCRSEFall %>% 
  group_by(COURSE_NAME) %>%
  summarise(Freq=n(), .groups="drop") %>% 
  arrange(desc(Freq)) %>% 
  mutate(Per=Freq*100/nrow(upCRSEFall))
upCRSEFallDT  
write.table(upCRSEFallDT,"COURSENAMEfreqlist.csv", sep=",")

upCRSEFallCRTSUM <- upCRSEFall %>% 
  group_by(UNIV_ROW_ID) %>%
  mutate(PSum=sum(STU_SECTN_CRED)) %>% 
  select(UNIV_ROW_ID,PSum) 

ComSumID <- upCRSEFallCRTSUM[!duplicated(upCRSEFallCRTSUM$UNIV_ROW_ID),] 
hist(ComSumID$PSum, 
     main="Histogram for Total Credit Hours of FTIC", 
     xlab="1st Fall Credit hours", 
     border="blue", 
     col="lightblue",
     xlim=c(3,20),
     las=1, 
     breaks=9)

boxplot.stats(ComSumID$PSum)
boxplot(ComSumID$PSum)

boxplot(ComSumID$PSum, 
     main="Boxplot for Total Credit Hours of FTIC", 
     xlab="1st Fall Credit hours", 
     border="blue", 
     col="lightblue"
     )



