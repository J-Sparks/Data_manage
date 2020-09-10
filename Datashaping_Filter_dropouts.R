### How to combine data sets and filter them with multiple variables
## import data sets 

#Imported dataset from folder Track_dropout_trends "dropoutFTIC_df1.csv"

myFTICdropoutdf1 <- dropoutFTIC_df1
summary(myFTICdropoutdf1)

library(dplyr)
m4df <- read.csv(file.choose(),header = T)#choose M4 
m4dfre <- select(m4df, "STU_ID", "Stu_CollegeBegin","Stu_BannerProgramCodeBegin",
                 "Stu_DepartmentBegin","Stu_Gender","Stu_Ethnicity",
                 "GPA_HIGHSCHOOL","Stu_Honors","Stu_InstitutionalGPA1",
                 "Stu_InstitutionalGPA2","Stu_ResidenceHallBegin","Stu_ResidenceHall1","FIRSTTERM",
                 "Stu_State")

# combine with dropout data "dropoutFTIC_df1.csv"

mydropout_moreinfo <- merge(mydropoutFTIC_df1, m4dfre, by = "STU_ID", all.x = TRUE)
View(mydropout_moreinfo)

glimpse(enr)
enrdropout <- select(enr, "STU_ID","Stu_CountyCode","Stu_County","Stu_StateCode","Stu_State")
dropoutbycounty <- merge(mydropout_moreinfo, enrdropout, by="STU_ID", all.x = TRUE)# duplicated
dropoutbycountynodu <- dropoutbycounty[!duplicated(dropoutbycounty$STU_ID),]

write.csv(dropoutbycountynodu,"V2DROPOUT_County.csv")

summary(dropoutbycountynodu)
dropdata <- filter(dropoutbycountynodu, dropoutbycountynodu$Cohort < 2018)
glimpse(dropdata)
attach(dropdata)

### Chi_test for independent
#H0: They are independent
#Ha: They are not independent
TAB = table(Stu_DepartmentBegin,IND_drop1)
TAB
barplot(TAB, beside = T, legend=F)
CHI <- chisq.test(TAB, correct =T)
CHI

TAB = table(Stu_Ethnicity,IND_drop1)
TAB
barplot(TAB, beside = T, legend=T)
CHI <- chisq.test(TAB, correct = T)
CHI

## filter FTIC 2018 FTIC UWF years HOurs
enrupdated <- read.csv(file.choose(),header = T)
FTIC2018ID <- select(dropout2018, 1:3)
enr201808to201908 <- filter(enrupdated, enrupdated$Stu_DEMO_TIME_FRAME>= 201808 )
myftic2018 <- filter(enr201808to201908, enr201808to201908$Stu_AdmissionRecentTypeCode =="B")
ftic2018terms <- select(myftic2018, "STU_ID","Stu_DEMO_TIME_FRAME","Stu_Term","Stu_TotalInstHours","Stu_TotalInstGradePoints")
ftic2018curremtTerm <- select(myftic2018, "STU_ID","Stu_DEMO_TIME_FRAME","Stu_Term", "Stu_CurrentTermLoad",
                              "Stu_GPATermHours","Stu_GPASemester")
#mergeFTIC2018
fticcurremtermandid <- merge(FTIC2018ID, ftic2018curremtTerm, by="STU_ID", by.all = T);glimpse(fticcurremtermandid)
my2018fticwithCurrenttermhours <- select(fticcurremtermandid, 1:2,5:6)
glimpse(my2018fticwithCurrenttermhours)
#spread data term key
widecurrentterm2018FT <- as.data.frame(spread(my2018fticwithCurrenttermhours, key= Stu_Term,value = Stu_CurrentTermLoad)); View(widecurrentterm2018FT)

## create  column for dropout index
library(dplyr)
widecurrentterm2018FT$Y1drop <- ifelse(widecurrentterm2018FT$`Fall 2019` > 0, "No", "Yes") # NA won't change to Yes
# for NA to "Yes"
widecurrentterm2018FT$Y1drop[is.na(widecurrentterm2018FT$Y1drop)] <- "Yes"

widecurrentterm2018FT[is.na(widecurrentterm2018FT)] <- 0

#conmbine all credit hours for 1 yaer and current term loaded
widecurrentterm2018FT$Y1sum <-(widecurrentterm2018FT$`Fall 2018`+widecurrentterm2018FT$`Spring 2019`+widecurrentterm2018FT$`Summer 2019`)
# data included county and Y1 GPA and demo
glimpse(dropoutbycountynodu)
myFTdemodata <- select(dropoutbycountynodu, 1:2,13:29)
myFT2018demodata <- filter(myFTdemodata, myFTdemodata$Cohort >= 2018)

##Merge with Term data and demo data
MyFT18all <- merge(widecurrentterm2018FT, myFT2018demodata, by = "STU_ID", all.x = T)
## I want to include UWF hours and TotalGradePoint from data name "ftic2018terms"
#need to filter for Fall 2019 only
FticFall2019termonly <- filter(ftic2018terms, ftic2018terms$Stu_Term == "Fall 2019")
#merge with FTIC2018
myFT18allGPA <- merge(MyFT18all, FticFall2019termonly, by = "STU_ID", all.x = TRUE)

##to get an drop out FT GPA
myFT18allGPAdropYes <- filter(myFT18allGPA, myFT18allGPA$Y1drop == "Yes")#201/30
myFT18allGPAdropNO <- filter(myFT18allGPA, myFT18allGPA$Y1drop == "No") #891/30

remyFT18allGPAdropYes <- select(myFT18allGPAdropYes, 1:26);glimpse(remyFT18allGPAdropYes)
allmyFTdropYesGPA <- merge(remyFT18allGPAdropYes, ftic2018terms, by="STU_ID", all.x = TRUE)# need to trim for recent term


#duplicated ID
dffdf <- allmyFTdropYesGPA  %>% 
  group_by(STU_ID) %>% 
  mutate(grouped_id = row_number())
dffdf

nodupli <- dffdf[!duplicated(dffdf$STU_ID),];nodupli # kept the first in row not for me

testdf2 <- group_by(dffdf, "STU_ID", "grouped_id") # creaded two columns in data set

by_ID <- dffdf %>% 
  filter(grouped_id == max(grouped_id));by_ID
all_ID <- select(by_ID, -31)


### need to combine two data sets
class(all_ID)
class(myFT18allGPAdropNO)
dataNo <- data.frame(myFT18allGPAdropNO)
dataYes <- data.frame(all_ID)
FT18Dropdata <- rbind(dataNo, dataYes)# final data set^^

## export the data
write.csv(FT18Dropdata, "DropoutFT18all.csv")

library(tidyr)
widefticterms <- spread(ftic2018terms, key=Stu_Term, value = Stu_TotalInstHours )
# combine with ftic2018 
FTIC2018UWFhors <- merge(FTIC2018ID, widefticterms, by="STU_ID", all.x = T);View(FTIC2018UWFhors)
