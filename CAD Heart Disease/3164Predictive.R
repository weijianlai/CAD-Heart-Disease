###########################################################################################################################
## File Name:       3164Predictive.R
## Project:         FIT 3164 Heart Disease Project
## Description:     The software's purpose is to create a predictive model
##                  that can predict the occurrence of CAD
## Date:            26/10/2020
## Author:          FIT 3164 Team 7
###########################################################################################################################



####################### Variable list description #########################################################################

## cad_revised    : Data frame that contains the top 10 best predictors that will be used in the final model.
## cad_test       : Data frame that contains data that will be used for testing the predictive model
## cad_train      : Data frame that contains data that will be used for training the predictive model
## cols.to.factor : List of binary values used to indicate which columns are considered as factors
## data           : The input data from user. Contains the file name of the data set
## df_cad         : Data frame that contains the transformed predictors and target variables to be used in neural network.
## df_temp        : A temporary data frame used during the creation of binary variables
## entropy_value  : Entropy value of the column
## feature.names  : List that contains all the variable names from the original data set
## levels         : variable that contains the number of levels in the specific factor column
## model_CI       : The confidence interval of the model
## model_revised  : The revised predictive neural network model with using the top 10 variables
## predict_model  : The neural network predictive model created using all the variables
## predictive_datasetset: Data frame containing the original data set.
## removeList     : A list that contains the variables to be removed (due to small variance)
## result         : the prediction list achieved by the model for the testing data
## size           : The number of rows in the dataset.
## train_control  : the training control variables used for the predictive model training.
## train.row      : A list containing the index of the rows used for the training dataset
## type           : A num used to execute the while loop.
## variable_list  : A variable list containing the top 10 variables to be used for the improved predictive model.



############################################################################################################################



#################################################### Library List ##########################################################

library(neuralnet)
library(readxl)
library(caret)
library(stringr)
library(entropy)
library(dplyr)
library(tools)

#############################################################################################################################


#Reading the dataset
type=1
while (type==1){
  #reading the data name
  data = readline("Enter the file name : ")

  #check if the type is xlsx and if it exists in the correct directory
  if (file_ext(data)!="xlsx"){
    message("Please enter an xlsx file")
  }
  else if (file.exists(data)!=TRUE){
    message("Please ensure that the file exists within the correct directory")
  }

  #check if the file has any datas (whether it has any rows)
  if(file_ext(data)=="xlsx" & file.exists(data)==TRUE){
    #loading the data into R
    predictive_dataset <- read_excel(data)
    size = dim(predictive_dataset)
    if (size[1]==0){
      message("Please ensure that the file is not empty")
    }
    #if the file pass all the tests, exit the while loop and accepts the input
    else{
      message("Data has been sucessfully accepted")
      type=0
    }
  }
}

#Remove the NULL value in the dataset
predictive_dataset = na.omit(predictive_dataset)

#Removing column with only a single value
predictive_dataset <- predictive_dataset %>% select_if(~n_distinct(.) > 1)

#Changing type to factor
cols.to.factor <- sapply( predictive_dataset, function(col) length(unique(col)) < 10)
predictive_dataset[cols.to.factor] <- lapply(predictive_dataset[ cols.to.factor] , factor)

#Removing variables with small variance (entropy)
removeList = vector()
for (i in 1:ncol(predictive_dataset)) {
  entropy_value = entropy(table(predictive_dataset[[i]]), base=exp(1))
  if (entropy_value<=0.1){
    removeList <- c(removeList, colnames(predictive_dataset[i]))
  }
}

if (length(removeList)>0){
  for (i in 1:length(removeList)){
    predictive_dataset[removeList[i]] <- list(NULL)
  }
}

#Renaming variables (replacing all spaces and strips with dots)
names(predictive_dataset) <- str_replace_all(names(predictive_dataset), c(" " = "." , "-" = "."))

#Creating binary variables for every categorical variables
temp_df= as.data.frame(subset(predictive_dataset, select=1))
df_cad = as.data.frame(model.matrix(~., data = temp_df))
df_cad[1] <- list(NULL)

for (i in 2:ncol(predictive_dataset)) {
  temp_df = as.data.frame(subset(predictive_dataset, select=i))
  temp_df = as.data.frame(model.matrix(~., data = temp_df))
  temp_df[1] <- list(NULL)
  df_cad = cbind(df_cad,temp_df)
}

#Changing the type back to factor
cols.to.factor <- sapply( df_cad, function(col) length(unique(col)) < 10)
df_cad[cols.to.factor] <- lapply(df_cad[ cols.to.factor] , factor)

#Changing the labels within the factor, so it can work with the neural network
feature.names=names(df_cad)
for (i in feature.names) {
  if (class(df_cad[[i]])=="factor") {
    levels <- unique(c(df_cad[[i]]))
    df_cad[[i]] <- factor(df_cad[[i]],
                          labels=make.names(levels))
  }
}

set.seed(1)
#Creating training and testing data
train.row = sample(1:nrow(df_cad), 0.7*nrow(df_cad))
cad_train = df_cad[train.row,]
cad_test = df_cad[-train.row,]

#training control for the model
train_control <- trainControl(method = 'cv', number = 35, classProbs = TRUE, verboseIter = TRUE, summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))

#Creation of the model
predict_model = train(cad_train[,1:ncol(cad_train)-1],cad_train[,ncol(cad_train)],method = 'nnet',trControl = train_control,  preProcess = c('center', 'scale'))

#Checking the accuracy and performance of the model
result = predict(predict_model, newdata = cad_test)
model_CI = confusionMatrix(result,cad_test[[ncol(cad_test)]],positive = NULL)

#Finding the top 10 most important variables
variable_list <-varImp(predict_model)
variable_list = variable_list$importance
variable_list$varName = row.names(variable_list)
variable_list = variable_list[order(-variable_list$Overall),][1:10,]
variable_list = variable_list$varName

#Create a data frame with only top 10 most important variables
cad_revised = df_cad[variable_list]
cad_revised = cbind(cad_revised,df_cad[,ncol(df_cad)])

#Rename the last (target) column to the original name
colnames(cad_revised)[ncol(cad_revised)] = colnames(df_cad[ncol(df_cad)])

#Creating training and testing data
train.row = sample(1:nrow(cad_revised), 0.7*nrow(cad_revised))
cad_train = cad_revised[train.row,]
cad_test = cad_revised[-train.row,]

#Revised predictive model using 10 most-important predictor
train_control <- trainControl(method = 'repeatedcv', number = 25, classProbs = TRUE, verboseIter = TRUE,
                              summaryFunction = twoClassSummary, preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))

model_revised = train(cad_train[,1:ncol(cad_train)-1],cad_train[,ncol(cad_train)],method = 'nnet',trControl = train_control,  preProcess = c('center', 'scale'))

#the new predicted result and confidence interval
result = predict(model_revised, newdata = cad_test)

model_CI = confusionMatrix(result,cad_test[[ncol(cad_test)]],positive = NULL)
