---
title: "Maching Learning Project"
author: "Mxu"
date: "Saturday, August 22, 2015"
output: html_document
---

##Read Train And Test Data
```{r}
train <- read.table("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",header=T, sep=",")
test <- read.table("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",header=T, sep=",")

```

##Clean Data
In this part, I did three part of cleaning:
1: remove columns that have NA
2: remove columns that have empty values
3: remove columns that have nothing related to class by common sense like timestamp or window
and did the same cleaning rule to test dataset
```{r, echo=T}
completecase<-data.frame()
for (i in 1:ncol(train)){
  checkna<-sum(complete.cases(train[,i]))
  completecase<-rbind(completecase,data.frame(i,checkna))
}

validcolumn<-completecase[completecase$checkna==19622,]
cleanData1<-train[,validcolumn$i]

noempty<-data.frame()
for (i in 1:ncol(cleanData1)){
  checkempty<-sum(cleanData1[,i]=="")
  noempty<-rbind(noempty,data.frame(i,checkempty))
}
noemptycolumn<-noempty[noempty$checkempty==0,]
cleanData2<-cleanData1[,noemptycolumn$i]

trainRemove <- grepl("^X|timestamp|window", names(cleanData2))
cleanData3 <- cleanData2[, !trainRemove]

testData1<-test[,validcolumn$i]
testData2<-testData1[,noemptycolumn$i]
testData3<-testData2[,!trainRemove]


```

##Data Slicing
```{r}
library(caret)
library(rpart)
set.seed(1234)
inTrain <- createDataPartition(cleanData3$classe, p=0.70, list=F)
trainData <- cleanData3[inTrain, ]
testData <- cleanData3[-inTrain, ]

```

##Fit Regression Model
```{r}

model<-train(classe~.,method="rf",data=trainData, proxy=T, ntree=50)
#system.time(train(classe~.,method="rf",data=trainData, proxy=T, ntree=50))
predict <- predict(model, testData)
confusionMatrix(testData$classe, predict)
outof_sample_error=1-0.9994
outof_sample_error

```

##Test Data Set Prediction
```{r}
test_predict<-predict(model, testData3)
```