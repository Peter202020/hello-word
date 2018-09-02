---
title: "Coursera - Practical Machine Learning"
output: html_document
---



###**Project description**
####Use data from accelometers on the belt forearm, arm and dumbell of 6 participants to predict the manner in which they did their exercises. For this purpose, variable "class" is to be used, together with other relevant variables.

###**Datasource**
####The training data: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
####The test data    : https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
####The data for this project come from: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har

###**Experiment architecture**
####Six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E).
####Class A corresponds to the specified execution of the exercise, while the other 4 classes correspond to common mistakes. Participants were supervised by an experienced weight lifter to make sure the execution complied to the manner they were supposed to simulate (http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har).

###**Prework**
####1.) Downloading data
####2.) Installing packages such as caret, randomForest, rpart, rpart.plot, RColorBrewer, rattle, e1071
####3.) setting seed in order to ensure reproducibility

###**Data cleaning**
####Loading packages and setting seed



```{r,message=FALSE, warning=FALSE}
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(e1071)

set.seed(777)
```

####Loading data into memory and replacing all missing values with "NA"
```{r}
training.data <- read.csv("pml-training.csv",na.strings=c("NA","#DIV/0!", ""))
testing.data <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!", ""))

```
####Using general exploratory data analysis commands to understand nature of data. 
```{r}
#number of observations and columns
dim(training.data) 

```

####str(training.data) reveals that many variables are with "NA" values, additionaly first 7 variables are not relevant for our project assignment, thus we can remove them.
####deleting columns if all values are "NA"
```{r}
training.data<-training.data[,colSums(is.na(training.data)) == 0]
testing.data <-testing.data[,colSums(is.na(testing.data)) == 0]
```

```{r}
training.data   <-training.data[,-c(1:7)]
testing.data <-testing.data[,-c(1:7)]
```
###**Split data for cross-validation**
####In order to verify functionality of the model, training.data are to be split into train.model data and test.model data in ration 80:20

```{r}
subsamples <- createDataPartition(y=training.data$classe, p=0.8, list=FALSE)
train.model <- training.data[subsamples, ] 
test.model <- training.data[-subsamples, ]
#number of observations and columns in train.model data
dim(train.model)
#number of observations and columns in test.model data
dim(test.model)
```

####Look at the variable "classe" in the train.model dataset in order to decide if further adjustment needed.
```{r}
summary(train.model$classe)
```
####Each classe A-E is represented by relativelly fair count of observations. Thus no further adjustment to perform.


###**Using prediction model: Random Forest**
```{r}
modelRF <- randomForest(classe ~. , data=train.model, method="class")

# Predicting - cross-validation
predictionRF <- predict(modelRF, test.model, type = "class")

# Test results on test.model data set - out of sample error
confusionMatrix(predictionRF, test.model$classe)

```


###**Using prediction model: Decision Tree**
```{r}
modelDT <- rpart(classe ~ ., data=train.model, method="class")

# Predicting - cross-validation
predictionDT <- predict(modelDT, test.model, type = "class")

# Test results on our test.model data set - out of sample error
confusionMatrix(predictionDT, test.model$classe)

                       
```
###Selecting prediction model
####From above, the random forest model performed better than the decision tree. 
####Accuracy 99.64% vs. 75.48%
####Out of sample error 0.36% vs.24.52%
####Hence, we use the random forest model for the final prediction.

###**Model in practice**

```{r}
runModel <- predict(modelRF, testing.data, type="class")
runModel
```