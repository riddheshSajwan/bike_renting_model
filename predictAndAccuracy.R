#predictions and accuracy for MLR

#for casual
casPredictionsMLR <- mlrmodelCas %>% predict(dsContCasTest)
casAccuracyMLR <- 100 - RMSE(casPredictionsMLR, dsContCasTest$casual)*100/mean(dsContCasTest$casual)
print(paste("Accuracy for casual ",casAccuracyMLR))
#for registered
regPredictionsMLR <- mlrmodelReg %>% predict(dsContRegTest)
regAccuracyMLR <- 100 - RMSE(regPredictionsMLR, dsContRegTest$registered)*100/mean(dsContRegTest$registered)
print(paste("Accuracy for registered ",regAccuracyMLR))
#predictions and accuracy for RF

#for casual 
casPredictionsRF <- rfmodelCas %>% predict(dsCatCasTest)
casAccuracyRF <- 100 - RMSE(casPredictionsRF, dsCatCasTest$casual)*100/mean(dsCatCasTest$casual)
print(paste("Accuracy for casual ",casAccuracyRF))
#for registered
regPredictionsRF <- rfmodelReg %>% predict(dsCatRegTest)
regAccuracyRF <- 100 - RMSE(regPredictionsRF, dsCatRegTest$registered)*100/mean(dsCatRegTest$registered)
print(paste("Accuracy for registered ",regAccuracyRF))
