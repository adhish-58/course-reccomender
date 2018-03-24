#install.packages("shiny")
library(shiny)
library(arules)
library(stringr)
library(datasets)

source("apriori_implementation.r", local = TRUE)

# Define UI for app that recommends courses
ui <- fluidPage(
  
  # App title ----
  titlePanel(h1("Hello, welcome to course recommender", align = "center")),
  br(),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      fluidRow(
      uiOutput("item1"),
      uiOutput("item2"),
      uiOutput("item3"),
      uiOutput("item4"),
      uiOutput("item5", align = "center")
      )
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      textOutput("intro_text"),
      br(),
      
      tabsetPanel(
        tabPanel("All Possible Courses", dataTableOutput("all_courses_db")),
        tabPanel("Recommended Courses",dataTableOutput("recommended_courses_db"))
      )
    )
  )
)

# Define server logic required to recommend courses
server <- function(input, output) {
  
  output$item1 <- renderUI({
      selectInput("uniSemester","Choose your Semester", c("Fall","Spring"," " ),selected=" ",selectize = TRUE)
    })
  output$item2 <- renderUI({
    selectInput("uniDegree","Choose your Degree", c("ECE","CS"," " ), selected= " ", selectize = TRUE)
    })
  output$item3 <- renderUI({
    selectInput("uniConcentration","Choose your Concentration", concentrations.list, selected= " ", selectize = TRUE)
    })
  output$item4 <- renderUI({
    selectInput("uniCourses","Enter your Courses Taken", all_courses$Full_Info, selected=" ",selectize = TRUE, multiple = TRUE)
    })
  br()
  output$item5 <- renderUI({
    actionButton("submit", style = "background: #0066CC; color: White", width = "100px", label = "Submit")
  })
  
  output$intro_text <- renderText({ 
    "List of All possible courses in your field of concentration and a list of reccomended courses"
    })
  
  ###################################################################################################
  
  #Creating the database of recommended courses
  output$recommended_courses_db <- renderDataTable({
    
    #create rul search based on the input semester, extracts rules as data frames
    if(input$uniSemester == "Fall"){
      courses.input <- input$uniCourses
      rules.search <- rules1.fall_courses[grep(paste(courses.input, collapse="|"), rules1.fall_courses$lhs),]
      rules.search1 <- as.data.frame(unique(rules.search[, 2]))
    }
    
    else{
      courses.input <- input$uniCourses
      rules.search <- rules1.spring_courses[grep(paste(courses.input, collapse="|"), rules1.spring_courses$lhs),]
      rules.search1 <- as.data.frame(unique(rules.search[, 2]))
    }  

    # create a matrix of the rule search so that it can be compared to the input courses
    recommended_courses_matrix <- as.matrix(rules.search1)
    rcm_1 <- dim(recommended_courses_matrix)[1]
    rcm_2 <- dim(recommended_courses_matrix)[2]
    rcm_final <- matrix(0:0 ,rcm_1, rcm_2)
    
    
    if(rcm_1!=0){
      for (i in 1:rcm_1){
        #compare inout courses with the rule search courses
        if(all(is.na(str_match(recommended_courses_matrix[i,1],input$uniCourses)))==TRUE){
          rcm_final[i,1 ] <- recommended_courses_matrix[i,1 ]
        }
      }
      
      rcm_final <- rcm_final[rcm_final[ ,1] != 0, ]
      rcm_final_data <- as.data.frame(rcm_final)

      #Display in the table      
      rcm_final_data[]
    }
      
    #Just display the rules as possible course recommendations if no inputs  
    else{
      rules.search1[]
    }
  })
  
  
  output$all_courses_db <- renderDataTable({
    
    #list for ECE 
    all_courses.ece <- switch(input$uniConcentration,
                      "BioEngineering" = courses.ece.concentrations[,1],
                      "Computer Systems and Software" = courses.ece.concentrations[,2],
                      "Digital Signal Processing" = courses.ece.concentrations[,3],
                      "Electrical Energy" = courses.ece.concentrations[,4],
                      "Electromagnetics" = courses.ece.concentrations[,5],
                      "Electronic Design and Applications" = courses.ece.concentrations[,6],
                      "Microelectronics" = courses.ece.concentrations[,7],
                      "Optics & Photonics" = courses.ece.concentrations[,8],
                      "Systems ad Controls" = courses.ece.concentrations[,9],
                      "Telecommunications" = courses.ece.concentrations[,10],
                      "VLSI" = courses.ece.concentrations[,11],
                      "Computation Perception and Robotics" = courses.ece.concentrations[,12],
                      "Computer Graphics" = courses.ece.concentrations[,13],
                      "Computing Systems" = courses.ece.concentrations[,14],
                      "Human Computer Interaction" = courses.ece.concentrations[,15],
                      "Interactive Intelligence" = courses.ece.concentrations[,16],
                      "Electronic Design and Applications" = courses.ece.concentrations[,17],
                      "Machine Learning" = courses.ece.concentrations[,18],
                      "Social Computing" = courses.ece.concentrations[,19],
                      "Visual Analytics" = courses.ece.concentrations[,20]
                      )
    
      #list for CS
      all_courses.cs <- switch(input$uniConcentration,
                        "Computation Perception and Robotics" = courses.cs.concentrations[,1],
                        "Computer Graphics" = courses.cs.concentrations[,2],
                        "Computing Systems" = courses.cs.concentrations[,3],
                        "Human Computer Interaction" = courses.cs.concentrations[,4],
                        "Interactive Intelligence" = courses.cs.concentrations[,5],
                        "Electronic Design and Applications" = courses.cs.concentrations[,6],
                        "Machine Learning" = courses.cs.concentrations[,7],
                        "Social Computing" = courses.cs.concentrations[,8],
                        "Visual Analytics" = courses.cs.concentrations[,9]
                        )
      
      #List of courses
      all_courses.ece.data <-as.data.frame(all_courses.ece)
      all_courses.cs.data <-as.data.frame(all_courses.cs)
      
      #Display courses based on the Degree
      if(input$uniDegree == "ECE"){
        all_courses.ece.data[]
      }
      
      else{
        all_courses.cs.data[]
      }
      
      })
  }

#Run the Web app
shinyApp(ui = ui, server = server) 
