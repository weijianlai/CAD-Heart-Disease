###########################################################################################################################
## File Name:       server.R 
## Project:         FIT 3164 Heart Disease Project
## Description:     The software's purpose is to create a predictive model 
##                  that can predict the occurrence of CAD
## Date:            26/10/2020
## Author:          FIT 3164 Team 7 
###########################################################################################################################



####################### Variable list description #########################################################################

## ha          : Matrix list that contains the 10 input variables for the predictive model
## df          : Data frame that contains the 10 input variables for the predictive model
## pred_prob   : ANN Predictive model
## name        : A variable to store the name of the user (input by the user)
## result      : A variable that display the final predictive result for the CAD


###################################### library ############################################################################
library(shiny)

###########################################################################################################################

## Load the predictive model, ANN model
load("ANN.rda")


fieldsMandatory <- c("age", "wbc", "tg", "esr")

shinyServer

## Create a function to make the mandatory variables mandatory to fill in. It will go through every variable declared in fieldsMandatory and make sure the input is not empty.
## It will check if the variables has any input before making the predict button available for the user. It will make the button pressable when the user input every 
## mandatory variable with a value.
(
  function(input,output,session){
    
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      shinyjs::toggleState(id = "success", condition = mandatoryFilled)
      
      
    })
    ## Set conditions for each input variables and join the variables from the UI with the server
    result<-eventReactive(input$success,
                          {
                            ha<-matrix(ncol = 10,c(input$sex,input$tcp,input$currentsmoker,input$dlp,input$nonanginal,input$dm,as.numeric(input$age),as.numeric(input$wbc),as.numeric(input$tg),as.numeric(input$esr)))
                            colnames(ha)<-c("SexMale","Typical.Chest.Pain1","Current.Smoker1","DLPY","NonanginalY","DM1","Age","WBC","TG","ESR")
                            
                            
                            # Check and ensure the input numerical data is in the correct format, range. It also ensure the user must input all the numerical data before allowing them to press the predict button.
                            validate(
                              need((input$age<=80 && input$age>=18), "Please enter a valid input! Age should be between 18 and 80.")
                            )
                            validate(
                              need((input$tg>=37 && input$tg<=1050), "Please enter a valid input! Triglyceride level should be between 37 and 1050.")
                            )
                            validate(
                              need((input$wbc<=18000 && input$wbc>=3700),"Please enter a valid input! White blood cell should be between 3700 and 18000.")
                            )
                            
                            validate(
                              need((input$esr<=90 && input$esr>=1),"Please enter a valid input! Erythrocyte sedimentation rate should between 1 and 90.")
                            )
                            
                            # Convert all the input variables into a data frame
                            df<-as.data.frame(ha)
                            
                            # Convert all the numerical variables into numerical form
                            df$Age = as.numeric(df$Age)
                            df$WBC = as.numeric(df$WBC)
                            df$TG = as.numeric(df$TG)
                            df$ESR = as.numeric(df$ESR)
                            
                            # Checking the accuracy and performance of the model
                            pred_prob<-predict(model_revised,df, method ='class', type="prob")
                       
                            # Get the fianl accuracy value in 3 significant figures and in percentage format.
                            signif(pred_prob[1]*100, 3)
                            
                            
                            
                          }) 
    
    
    # Output the result. This can be called in the UI to display the result.
    output$text<-renderText({
      name = paste("Hi ",input$name, ", ")
      
      result = paste(name,"You have a probablity of ", result(),"% of having CAD. ")
      
      HTML(paste(result))
    })
    
    
  })