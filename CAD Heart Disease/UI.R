###########################################################################################################################
## File Name:       3164Predictive.R 
## Project:         FIT 3164 Heart Disease Project
## Description:     The software's purpose is to create a predictive model 
##                  that can predict the occurrence of CAD
## Date:            26/10/2020
## Author:          FIT 3164 Team 7
###########################################################################################################################



#######################Variable list description###########################################################################

##fieldsMandatory: a list contains all the required fileds name

###########################################################################################################################



#######################Library List########################################################################################

library(shiny)
library(shinydashboard)

###########################################################################################################################

#define the required fields
fieldsMandatory <- c("age", "wbc", "tg", "esr")

#add asterisk next to all the required fileds
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

#make the asterisk color to red
appCSS <- ".mandatory_star { color: red; }"

#design of UI
shinyUI(dashboardPage(dashboardHeader(title = 'CAD Heart Disease Prediction', titleWidth = 250), 
                      
                      #design of the menu bar
                      dashboardSidebar(width=150,
                                       #first item in menuItem is what is shown on the UI, second item of menuItem
                                       #is the menu name used later to connect with the page content
                                       sidebarMenu(menuItem("CAD Introduction", tabName="intro",icon=icon("file-alt")),
                                                   menuItem("Prediction",tabName="prediction",icon=icon("poll")))),
                      
                      #design contents of each menu in the menu bar
                      dashboardBody(
                        #error message font color and font size
                        tags$head(
                          tags$style(HTML("
                            .shiny-output-error-validation {
                              color: #ff0000;
                              font-size: 100%;
                            }
                          "))
                        ),               
                        
                        #design of contents of two pages
                        tabItems(
                          
                          #first page shows the CAD introduction, connected with menu 'intro'
                          tabItem(tabName = "intro",
                                  #arrange box line by line
                                  fluidRow(
                                    
                                    # a box contains the introduction of CAD
                                    box(title="What is CAD?",status="warning",width=10,height=145,solidHeader = TRUE,
                                        p("Cardiovascular disease is one of the leading causes of death around the world.",
                                          "Coronary artery disease occurs when cholesterol and fatty deposits attach themselves to the wall of arteries,",
                                          "which would block the arteries and restrict the blood flow. The heart could not function properly",
                                          "as it needs more oxygen and nutrients which would lead to angina.")),
                                    
                                    # a box contains suggestions to lower the risk of CAD
                                    box(title="Suggestions to lower the risk of CAD",status="info",width=10,height=240,solidHeader = TRUE,
                                        p(div(),
                                          "1. Control your blood pressure",
                                          div(),
                                          "2. Maintain a healthy weight",
                                          div(),
                                          "3. Eat healthily",
                                          div(),
                                          "4. Excercise regularly",
                                          div(),
                                          "5. Reduce alcohol consumption",
                                          div(),
                                          "6. Manage stress",
                                          div(),
                                          "7. Get enough sleep")))),
                          
                          #second page includes prediction inputs and results
                          tabItem(tabName = 'prediction',
                                  
                                  shinyjs::useShinyjs(),
                                  shinyjs::inlineCSS(appCSS),
                                  fluidRow(
                                    
                                    #categorical variables box
                                    box(title='Categorical Variables',status='primary',width=12, height=145, solidHeader = TRUE,
                                        # box layout
                                        splitLayout(
                                          #change the dropdown menu style to visible
                                          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow:visible;}"))),
                                          #radio button size, text size, space size
                                          cellWidths = c('0%','12%','3%','12','8%','12%', '4%', '12%', '2%', '12%','1%','12%', '1%', '12%'),
                                          textInput("name", "Enter your name:",""),
                                          div(),
                                          radioButtons("sex", "Sex:", choices = c("Male" = "X1", "Female" = "X2"), selected = "X1"),
                                          div(),
                                          radioButtons("tcp", "Typical Chest Pain:", choices = c("No" = "X1", "Yes" = "X2"), selected = "X1"),
                                          div(),
                                          radioButtons("currentsmoker", "Current Smoker:", choices = c("No" = "X2", "Yes" = "X1"), selected = "X2"),
                                          div(),
                                          radioButtons("dlp", "Dyslipidermia:", choices = c("No" = "X2", "Yes" = "X1"), selected = "X2"),
                                          div(),
                                          radioButtons("nonanginal", "Nonanginal CP:", choices = c("No" = "X1", "Yes" = "X2"), selected = "X1"),
                                          div(),
                                          radioButtons("dm", "Diabetes Mellitus:", choices = c("No" = "X1", "Yes" = "X2"), selected = "X1")))),
                                  
                                  #numerical variables box
                                  box(title='Numerical Variables',status='primary', width=27, height = 145, solidHeader = TRUE,
                                      #deesign the layout of the box
                                      splitLayout(
                                        #define each input box size and space between them
                                        cellWidths=c('12%', '2%','12%', '12%', '12%', '12%', '12%'),
                                        numericInput("age", labelMandatory("Age:"),min = 0,max = 100,value = 70),
                                        div(),
                                        numericInput("wbc", labelMandatory("White Blood Cell(cells/mL):"),min = 0, max = 100, value = 7000),
                                        div(),
                                        numericInput("tg", labelMandatory("Triglyceride level(mg/dL):"),min = 0, max = 120, value = 90),
                                        div(),
                                        numericInput("esr", labelMandatory("Erythrocyte Sedimentation Rate(mm/h):"),min = 0, max = 100, value = 60))),
                                  
                                  #button to do prediction
                                  actionButton('success','Predict', class = "btn-primary"),
                                  p("*We will not disclose your personal information"),
                                  div(),
                                  
                                  #result box where shows the error message or prediction result
                                  box(strong("Result"),' (Please enter all required fields to get the result)',status='success',
                                      solidHeader = TRUE,width=150,height=80,h4(strong(textOutput("text"))),collapse = '<br/>'),
                                  p('*The prediction result is 85% accurate on average'),
                                  
                                  #a reference box to show the average level of numerical variables
                                  box(title='Average level of numeric variables above (only for a reference)', status="info", 
                                      width=27, height = 150, solidHeader = TRUE,
                                      p(div(),
                                        "Age: 58.90",
                                        div(),
                                        "White Blood Cell: 7562.05",
                                        div(),
                                        "Triglyceride level: 150.34",
                                        div(),
                                        "Erythrocyte Sedimentation Rate: 19.46")
                                      
                                  )
                                  
                          ))
                        
                      )
)
)