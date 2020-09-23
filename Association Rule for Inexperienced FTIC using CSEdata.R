
#####################################################################
#### Association Rule for Inexperienced FTIC using CSEdata  ######
#####################################################################




library(readr)
CSEdata <- read_csv("~/Data_IR/CSEdata.csv", 
                    col_types = cols(UNIV_ROW_ID = col_character()))
View(CSEdata)
glimpse(CSEdata)
apply(CSEdata, 2, p)

outliersAPR <- CSEdata$GPA_ENTERING_SECOND_FALL
outL <- boxplot.stats(CSEdata$GPA_ENTERING_SECOND_FALL)$out 
outL# 0.990000 2.525735 3.106896 3.555719 4.000000
min(outL, na.rm=TRUE) #0
max(outL, na.rm = TRUE) #0.98
glimpse(CSEdata)
### Filter not outliers

CSEdataV1 <- CSEdata %>% 
  filter(GPA_ENTERING_SECOND_FALL >= 0.99, na.rm=TRUE )  %>% # remove outliers
  mutate(AverageGPAindex = ifelse(GPA_ENTERING_SECOND_FALL< 3.106896, "Below3.10","Above3.10")) %>% 
  mutate(ExpFTIC=ifelse(HOURS_BROUGHT_TO_UNIVERSITY<= 0,"InexpFTIC","ExpFTIC"))
apply(CSEdataV1, 2, p)   # no NA for GPA

FTIC_GPADEG_com <- CSEdataV1 %>% 
  group_by(COHORT_YEAR,ExpFTIC,AverageGPAindex) %>% 
  summarise(meanPriorHrs=mean(HOURS_BROUGHT_TO_UNIVERSITY),CountFTIC=n())  #(meanGPA = mean(GPA1stFall),
FTIC_GPADEG_com



#################################
######## select variables #######
#################################

CSEdataV2 <-  CSEdataV1 %>% 
  #filter(ExpFTIC=="InexpFTIC") %>% 
  select(ENTRY_COLLEGE,
         ENTRY_DEPARTMENT,
         GENDER,
         ETHNICITY,
         COUNTY,
         ENTRY_PROGRAM,
         #COUNTY,
         FIRST_GENERATION_STUDENT,
         #ATHLETE,
         APPLICANT_TIER,
         HIGH_SCHOOL_NAME,
         GPA_HIGHSCHOOL,
         HOURS_BROUGHT_TO_UNIVERSITY,
         AP_CREDITS,
         ACT_PROPORTION,
         SAT_PROPORTION,
         #FIRST_FALL_PELL,FIRST_FALL_BRIGHT_FUTURES,FIRST_FALL_ANY_LOANS,
         #FIRST_FALL_NEED_BASED_LOANS,
         AverageGPAindex)

summary(CSEdataV2)
# need libraries
library(arules)
library(arulesViz)


#summary
CSEdataV3 <- as(CSEdataV2,"transactions") #transaction are analysed to identify rules of assocation
class(CSEdataV3)

summary(CSEdataV3)
#inspect(head(newGPAassociationDF)) # too long
itemFrequencyPlot(CSEdataV3, topN=10)
itemFrequencyPlot(CSEdataV3, support=0.05)

# results for above3.10
rules_above <- apriori(CSEdataV3,parameter = list(minlen=1, maxlen=6, conf=.5, support=0.1), # to see whether there is sufficient evidence to suggest
                          appearance = list(rhs=c("AverageGPAindex=Above3.10"), default = "lhs"))
results_above <- inspect(head(sort(rules_above, by="lift"),10))
results_above


# results for below3.10
rules_below <- apriori(CSEdataV3,parameter = list(minlen=1, maxlen=6, conf=.5, support=0.2), # to see whether there is sufficient evidence to suggest
                       appearance = list(rhs=c("AverageGPAindex=Below3.10"), default = "lhs"))
results_below <- inspect(head(sort(rules_below, by="lift"),10))
results_below