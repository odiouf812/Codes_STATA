data <- read.csv(file.choose(), header = TRUE)

#Reading data into R
data<- read.csv("/Users/Zulaikha_Geer/Desktop/NaiveBayesData/diabetes.csv")

#Loading required packages
install.packages('tidyverse')
library(tidyverse)
install.packages('ggplot2')
library(ggplot2)
install.packages('caret')
library(caret)
install.packages('caretEnsemble')
library(caretEnsemble)
install.packages('psych')
library(psych)
install.packages('Amelia')
library(Amelia)
install.packages('mice')
library(mice)
install.packages('GGally')
library(GGally)
install.packages('rpart')
library(rpart)
install.packages('randomForest')
library(randomForest)


###### Before we study the data set let’s convert the output variable (‘Outcome’) into categorical variables. This is necessary because our output will be in the form of 2 classes, True or False. Where true will denote that a patient has diabetes and false denotes that a person is diabetes free.

#Setting outcome variables as categorical 

data$Outcome <- factor(data$Outcome, levels = c(0,1), labels = c("False", "True"))

#Studying the structure of the data 

str(data)

head(data)

describe(data)

######### While analyzing the structure of the data set, we can see that the minimum values for Glucose, Bloodpressure, Skinthickness, Insulin, and BMI are all zero. This is not ideal since no one can have a value of zero for Glucose, blood pressure, etc. Therefore, such values are treated as missing observations.

In the below code snippet, we’re setting the zero values to NA’s:

#Convert '0' values into NA
data[, 2:7][data[, 2:7] == 0] <- NA

######## To check how many missing values we have now, let’s visualize the data:

#visualize the missing data
missmap(data)

##### The above illustrations show that our data set has plenty missing values and removing all of them will leave us with an even smaller data set, therefore, we can perform imputations by using the mice package in R.

#Use mice package to predict missing values
mice_mod <- mice(data[, c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")], method='rf')
mice_complete <- complete(mice_mod)

missmap(data)

####### Now let’s perform a couple of visualizations to take a better look at each variable, this stage is essential to understand the significance of each predictor variable.

#Data Visualization
#Visual 1
ggplot(data, aes(Age, colour = Outcome)) +
geom_freqpoly(binwidth = 1) + labs(title=”Age Distribution by Outcome”)

ggpairs(data)

###################### This stage begins with a process called Data Splicing, wherein the data set is split into two parts:

#Building a model
#split data into training and test data sets
indxTrain <- createDataPartition(y = data$Outcome,p = 0.75,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]
 
#Check dimensions of the split
 
> prop.table(table(data$Outcome)) * 100
 
False True
65.10417 34.89583
 
> prop.table(table(training$Outcome)) * 100
 
False True
65.10417 34.89583
 
> prop.table(table(testing$Outcome)) * 100
 
False True
65.10417 34.89583

############### For comparing the outcome of the training and testing phase let’s create separate variables that store the value of the response variable:

create objects x which holds the predictor variables and y which holds the response variables
x = training[,-9]
y = training$Outcome

