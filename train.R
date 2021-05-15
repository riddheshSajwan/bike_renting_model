library(tidyverse)
library(randomForest)
library(caret)

#index for partition: test and train
trainIndex = createDataPartition(dsOutRemoved$count, p = .80, list = FALSE)

#multiple regression model for continuous variables

#for casual
dsContCas <- dsOutRemoved[trainIndex,1:11]
dsContCasTest <- dsOutRemoved[-trainIndex,1:11]

#removing season due to feature selection chi-test
dsContCas$season <- NULL 
dsContCasTest$season <- NULL 
mlrmodelCas <- lm(casual ~., data = dsContCas)
summary(mlrmodelCas)

#for registered
dsContReg <- dsOutRemoved[trainIndex,1:10]
dsContRegTest <- dsOutRemoved[-trainIndex,1:10]
dsContReg$registered <- dsOutRemoved[trainIndex,13]
dsContRegTest$registered <- dsOutRemoved[-trainIndex,13]
mlrmodelReg <- lm(registered ~., data = dsContReg)
summary(mlrmodelReg)

#random forest for categorical variables 

#for casual
dsCatCas <- dsOutRemoved[trainIndex,1:11]
dsCatCasTest <- dsOutRemoved[-trainIndex,1:11]

#removing season due to feature selection chi-test
dsCatCas$season <- NULL
dsCatCasTest$season <- NULL

#keeping categorical variables as categorical
dsCatCas <- transform(dsCatCas,
                      year = as.factor(year),
                      month = as.factor(month),
                      holiday = as.factor(holiday),
                      weekday = as.factor(weekday),
                      workingday = as.factor(workingday),
                      weatherSituation = as.factor(weatherSituation))
dsCatCasTest <- transform(dsCatCasTest,
                      year = as.factor(year),
                      month = as.factor(month),
                      holiday = as.factor(holiday),
                      weekday = as.factor(weekday),
                      workingday = as.factor(workingday),
                      weatherSituation = as.factor(weatherSituation))
rfmodelCas <- randomForest(casual ~ ., data = dsCatCas)
print(rfmodelCas)

#for registered
dsCatReg <- dsOutRemoved[trainIndex,1:10]
dsCatRegTest <- dsOutRemoved[-trainIndex,1:10]
dsCatReg$registered <- dsOutRemoved[trainIndex,13]
dsCatRegTest$registered <- dsOutRemoved[-trainIndex,13]

#keeping categorical variables as categorical
dsCatReg <- transform(dsCatReg,
                      season = as.factor(season),
                      year = as.factor(year),
                      month = as.factor(month),
                      holiday = as.factor(holiday),
                      weekday = as.factor(weekday),
                      workingday = as.factor(workingday),
                      weatherSituation = as.factor(weatherSituation))
dsCatRegTest <- transform(dsCatRegTest,
                      season = as.factor(season),
                      year = as.factor(year),
                      month = as.factor(month),
                      holiday = as.factor(holiday),
                      weekday = as.factor(weekday),
                      workingday = as.factor(workingday),
                      weatherSituation = as.factor(weatherSituation))
rfmodelReg <- randomForest(registered ~ ., data = dsCatReg)
print(rfmodelReg)

