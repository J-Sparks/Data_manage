# Read data
library(caret)
library(naivebayes)
library(readr)
library(dplyr)
FTIC1718HMCSEDropout_DF_Completed_HSGPA <- 
      read_csv("C:/Users/jk99/Desktop/DATA 202001/FTIC1718HMCSEDropout_DF_Completed_HSGPA.csv", 
                                                    col_types = cols(X1 = col_skip()))
Dropout <- FTIC1718HMCSEDropout_DF_Completed_HSGPA[,-1]# stu_id
Dropout <- Dropout[,-1]# UWFGPA1st Year



# missing values

glimpse(Dropout)

#select original datset
allDropout <- select(Dropout, "Prior_Hours","GPA_HIGHSCHOOL","StartMajor","Gender","Ethnicity","County","FirstMathCrs",
                  "FirstMathCrsGrade","UWFHour1stTerm","UWFGPA1stTerm","Drop1stYear")

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(allDropout$Drop1stYear, p=0.8, list = FALSE)
TrainingSet <- allDropout[TrainingIndex,] # Training Set
TestingSet <- allDropout[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1] #row number
TrainSet <- TrainSet[,-1] # STU_ID

# Build model using all factors and labells

#model <- naive_bayes(Dropouts ~ ., data = Dropout)
allmodelNB <- naive_bayes(Drop1stYear ~ ., data = TrainSet, usekernel = TRUE)
allmodelNB
tables(allmodelNB)
#
#accuracy 86.7% used kernel
confMatRank <- table(predict(allmodelNB), TrainSet$Drop1stYear)
confMatRank
sum(diag(confMatRank)/sum(confMatRank)) # 0.8657534
1-sum(diag(confMatRank)/sum(confMatRank)) 
p <- predict(allmodelNB, TrainSet)
pred <- predict(allmodelNB, TrainSet[c(1:10),], type="prob")
pred
n <- table(TrainSet$Drop1stYear)
n
n/sum(n)




#Usng fewer factors 

re_Dropout <- select(Dropout, "GPA_HIGHSCHOOL","Prior_Hours",
                     "FirstMathCrs","FirstMathCrsGrade","UWFHour1stTerm","UWFGPA1stTerm","Drop1stYear")

TrainingIndex <- createDataPartition(re_Dropout$Drop1stYear, p=0.8, list = FALSE)
TrainingSet <- re_Dropout[TrainingIndex,] # Training Set
TestingSet <- re_Dropout[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1] #row number
#TrainSet <- TrainSet[,-1] # STU_ID
TestSet <- read.csv("testing.csv", header = TRUE)
TestSet <- TestSet[,-1]
# Build model
#model <- naive_bayes(Dropouts ~ ., data = Dropout)
re_modelNB <- naive_bayes(Drop1stYear ~ ., data = TrainSet, usekernel = TRUE)
re_modelNB
tables(re_modelNB)

#accuracy 83.69% used kernel
confMatRank <- table(predict(re_modelNB), TrainSet$Drop1stYear)
confMatRank
sum(diag(confMatRank)/sum(confMatRank)) 
1-sum(diag(confMatRank)/sum(confMatRank)) 
p <- predict(re_modelNB, TrainSet)
pred <- predict(re_modelNB, TrainSet[c(1:10),], type="prob")
pred
n <- table(TrainSet$Drop1stYear)
n
n/sum(n)


#testset
re_modelNBTest<- naive_bayes(Drop1stYear ~ ., data = TestSet)
confMatRank <- table(predict(re_modelNBTest), TestSet$Drop1stYear)
confMatRank
sum(diag(confMatRank)/sum(confMatRank)) 
1-sum(diag(confMatRank)/sum(confMatRank)) 




#import data reFTIC1718HMCSEDropout_DF_Completed_HSGPA
FTIC1718HMCSEDropout_DF_Completed_HSGPA <- 
  read_csv("C:/Users/jk99/Desktop/DATA 202001/ReFTIC1718HMCSEDropout_DF_Completed_HSGPA.csv" 
           )
Dropout <- FTIC1718HMCSEDropout_DF_Completed_HSGPA[,-1]# stu_id
Dropout <- Dropout[,-1]# UWFGPA1st Year

#select columns that filtered >= 10 stu and relabelled

re_Dropout <- select(Dropout,"GPA_HIGHSCHOOL", "Prior_Hours","ReStartMajor",
                     "Gender","ReEthnicity","ReCounty","ReFirstMathCrs",
                     "FirstMathCrsGrade","UWFHour1stTerm","UWFGPA1stTerm","Drop1stYear")

TrainingIndex <- createDataPartition(re_Dropout$Drop1stYear, p=0.8, list = FALSE)
TrainingSet <- re_Dropout[TrainingIndex,] # Training Set
TestingSet <- re_Dropout[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1] #row number
#TrainSet <- TrainSet[,-1] # STU_ID
TestSet <- read.csv("testing.csv", header = TRUE)
TestSet <- TestSet[,-1]
# Build model
#model <- naive_bayes(Dropouts ~ ., data = Dropout)
library(naivebayes)
re_modelNB <- naive_bayes(Drop1stYear ~ ., data = TrainSet, usekernel = TRUE)
re_modelNB
#tables(re_modelNB)

#accuracy improved 87.12% used kernel
confMatRank <- table(predict(re_modelNB), TrainSet$Drop1stYear)
confMatRank
sum(diag(confMatRank)/sum(confMatRank)) #0.8712329
1-sum(diag(confMatRank)/sum(confMatRank)) 
p <- predict(re_modelNB, TrainSet)
pred <- predict(re_modelNB, TrainSet[c(1:10),], type="prob")
pred
n <- table(TrainSet$Drop1stYear)
n
n/sum(n)





# Save model to RDS file
saveRDS(re_modelNB, "model.rds")
# Read in the RF model
model <- readRDS("model.rds")


####################################
# User interface                   #
####################################
library(shiny)
library(shinythemes)
library(data.table)
ui <- fluidPage(theme = shinytheme("superhero"),
                
                # Page header
                headerPanel('How likely to dropout?'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  
                  sliderInput("GPA_HIGHSCHOOL", label = "GPA_HIGHSCHOOL:",
                              min = min(TrainSet$GPA_HIGHSCHOOL),
                              max = max(TrainSet$GPA_HIGHSCHOOL),
                              value = 4.2),
                  sliderInput("Prior_Hours", "Prior_Hours:",
                              min = min(TrainSet$Prior_Hours),
                              max = max(TrainSet$Prior_Hours),
                              value = 12),
                  selectInput("ReStartMajor", label = "StartMajor:", 
                              choices = list( "Natural Science" = "Natural Science",             
                                              "Software Engineering" = "Software Engineering",      
                                              "Mechanical Engineering" = "Mechanical Engineering",    
                                              "Biomedical Sciences" =  "Biomedical Sciences",       
                                              "Marine Biology" =  "Marine Biology",             
                                              "Cybersecurity"  = "Cybersecurity",             
                                              "Engineering" =  "Engineering",              
                                              "Software Design & Development"="Software Design & Development",
                                              "Computer Science"= "Computer Science",          
                                              "Biology" = "Biology",                   
                                              "Electrical Engineering" = "Electrical Engineering",  
                                              "Computer Engineering"=  "Computer Engineering",       
                                              "Biochemistry" = "Biochemistry",             
                                             "Others" = "Others",                     
                                             "Environmental Management"="Environmental Management",    
                                             "Chemistry" = "Chemistry",                 
                                             "Information Technology"= "Information Technology",      
                                             "Physics"  = "Physics",                   
                                             "Mathematics"="Mathematics" ), 
                              selected = "Mathematics"),
                  selectInput("Gender", label = "Gender:", 
                              choices = list("Male"="M", "Female"="F"), 
                              selected = "Male"),
                  
                  selectInput("ReEthnicity", label = "Ethnicity:", 
                              choices = list( "White" = "White",        
                                              "Two or More" = "Two or More",   
                                              "Hispanic" = "Hispanic",     
                                              "African American" = "African American",
                                              "Asian" = "Asian" ,         
                                              "Not Reported" = "Not Reported",   
                                              "Others" = "Others" ), 
                               selected = "White"),
                  selectInput("ReCounty", label = "County:", 
                              choices = list( "OKAL" = "OKAL", "ESCA" = "ESCA",
                                              "Others" = "Others", "SANT" ="SANT",
                                              "NFLA" = "NFLA",
                                              "WALT" = "WALT" , "ORAN" = "ORAN",
                                              "BAY" =  "BAY",  "PASC" = "PASC",
                                              "DUVA"= "DUVA",
                                              "HILL" = "HILL"), 
                              selected = "OKAL"),
                  selectInput("ReFirstMathCrs", label = "FirstMathCrs:", 
                              choices = list("MAC1105"="MAC1105",
                                             "MAC1105C"="MAC1105C",
                                             "MAC1140"="MAC1140",
                                             "MAC1147"="MAC1147",
                                             "MAC2311"="MAC2311",
                                             "MAC2312"="MAC2312",
                                             "NoAttempt"="NoAttempt",
                                             "Others"="Others"), 
                              selected = "MAC1147"),
                  selectInput("FirstMathCrsGrade", label = "FirstMathCrsGrade:", 
                              choices = list("A"="A","A-"="A-","B"="B","B+"="B+","B-"="B-",
                                             "C+"="C+","C"="C","C-"="C-","D"="D","D+"="D+",
                                             "NoAttempt"="NoAttempt","W"="W","NF"="NF","F"="F"), 
                              selected = "C+"),
                  sliderInput("UWFHour1stTerm", "UWFHour1stTerm:",
                              min = min(TrainSet$UWFHour1stTerm),
                              max = max(TrainSet$UWFHour1stTerm),
                              value = 10), 
                  sliderInput("UWFGPA1stTerm", "UWFGPA1stTerm:",
                              min = min(TrainSet$UWFGPA1stTerm),
                              max = max(TrainSet$UWFGPA1stTerm),
                              value = 2.0),
                 
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    
    df <- data.frame(
      Name = c("GPA_HIGHSCHOOL",
               "Prior_Hours",
               "ReStartMajor",
               "Gender",
               "ReEthnicity",
               "ReCounty",
               "ReFirstMathCrs",
               "FirstMathCrsGrade",
               "UWFHour1stTerm",
               "UWFGPA1stTerm"
               ),
      Value = as.character(c(input$GPA_HIGHSCHOOL,
                             input$Prior_Hours,
                             input$ReStartMajor,
                             input$Gender,
                             input$ReEthnicity,
                             input$ReCounty,
                             input$ReFirstMathCrs,
                             input$FirstMathCrsGrade,
                             input$UWFHour1stTerm,
                             input$UWFGPA1stTerm
                             )),
      stringsAsFactors = FALSE)
    
    Drop1stYear <- 0
    df <- rbind(df, Drop1stYear)
    input <- transpose(df)
    
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    test$ReFirstMathCrs <- factor(test$FirstMathCrs, levels = c("MAC1105","MAC1105C","MAC2311","MAC1140","MAC1147","Others",
                                                              "MAC2312","NoAttempt"))
    test$FirstMathCrsGrade <- factor(test$FirstMathCrsGrade, levels = c("A"  ,       "B"  ,       "D"  ,       "NoAttempt",
                                                                        "C-"    ,    "B+"   ,     "B-"   ,     "C+"   ,    
                                                                         "A-"   ,     "W"  ,       "C" ,        "F"  ,      
                                                                        "NF","D+"))
    test$ReEthnicity <- factor(test$ReEthnicity, levels = c("White" , "Hispanic",    
                                                            "Two or More",      "African American",
                                                            "Others",           "Asian",           
                                                            "Not Reported"))
    
    test$Gender <- factor(test$Gender, levels=c("M","F"))
    
    Output <- data.frame(Prediction=round(predict(re_modelNB,test,type="prob"), 3))
    print(Output)
  })
  
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Please,ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)

