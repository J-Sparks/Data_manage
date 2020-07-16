library(readr)
re_re_re_Dropout <- read_csv("C:/Users/jk99/Desktop/DATA 202001/re_re_re_Dropout.csv", 
                             col_types = cols(Drop1stYear = col_factor(levels = c()), 
                                              ReFirstMathCrs = col_factor(levels = c()), 
                                              ReFirstMathCrsGrade = col_factor(levels = c()), 
                                              X1 = col_skip()))
TrainingIndex <- createDataPartition(re_re_re_Dropout$Drop1stYear, p=0.8, list = FALSE)
TrainingSet <- re_re_re_Dropout[TrainingIndex,] # Training Set
TestingSet <- re_re_re_Dropout[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1] 
re_modelNB <- naive_bayes(Drop1stYear ~ ., data = TrainSet)


summary(re_modelNB)
confMatRank <- table(predict(re_modelNB), TrainSet$Drop1stYear)
confMatRank
sum(diag(confMatRank)/sum(confMatRank)) ### 82% accuracy
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

ui <- fluidPage(theme = shinytheme("united"),
                
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
                  selectInput("ReFirstMathCrs", label = "ReFirstMathCrs:", 
                              choices = list("MAC1105/C"="MAC1105/C",
                                             "MAC1140/14"="MAC1140/14",
                                             "MAC1147"="MAC1147",
                                             "MAC2311/UP"="MAC2311/UP",
                                             "NoAttempt"="NoAttempt"), 
                              selected = "MAC1147"),
                  selectInput("ReFirstMathCrsGrade", label = "ReFirstMathCrsGrade:", 
                              choices = list("A_range"="A_range",
                                             "B_range"="B_range",
                                             "C_range"="C_range",
                                             "Failed"="Failed",
                                             "NoAttempt"="NoAttempt",
                                             "Withdrawn"="Withdrawn"), 
                              selected = "NoAttempt"),
                  
                  sliderInput("UWFHour1stTerm", "UWFHour1stTerm:",
                              min = min(TrainSet$UWFHour1stTerm),
                              max = max(TrainSet$UWFHour1stTerm),
                              value = 10), 
                  sliderInput("UWFGPA1stTerm", "UWFGPA1stTerm:",
                              min = min(TrainSet$UWFGPA1stTerm),
                              max = max(TrainSet$UWFGPA1stTerm),
                              value = 2.9),
                  
                  
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
               "ReFirstMathCrs",
               "ReFirstMathCrsGrade",
               "UWFHour1stTerm",
               "UWFGPA1stTerm"
      ),
      Value = as.character(c(input$GPA_HIGHSCHOOL,
                             input$Prior_Hours,
                             input$ReFirstMathCrs,
                             input$ReFirstMathCrsGrade,
                             input$UWFHour1stTerm,
                             input$UWFGPA1stTerm
      )),
      stringsAsFactors = FALSE)
    
    Drop1stYear <- 0
    df <- rbind(df, Drop1stYear)
    input <- transpose(df)
    
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    test$FirstMathCrs <- factor(test$ReFirstMathCrs, levels = c("MAC1147","MAC2311/UP",
                                                                "MAC1105/C",  "NoAttempt", 
                                                               "MAC1140/14"))
    test$FirstMathCrsGrade <- factor(test$ReFirstMathCrsGrade, levels = c( "A_range",   "B_range",  
                                                                          "Failed",    "NoAttempt",
                                                                          "C_range",   "Withdrawn"))
    
    
    
    
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
