library(mlbench)
library(caret)
library(gridExtra)

#dataframe with continuos variables
dsCont <- dsOutRemoved[,8:11]
#correlation analysis
corrMat <- cor(dsCont)

#printing out png of correlation matrix
png("correlationMatrix.png", height = 200, width = 600)
grid.table(corrMat)
dev.off()

#finding highly correlated
highCorr <- findCorrelation(corrMat, cutoff=0.65)
print(highCorr)

#dataframe with categorical variables
dsCat <- dsOutRemoved[,1:7]
#making function
chiTestFunc <- function(df,var){
  j <- grep(var,colnames(dsOutRemoved))
  for (i in 1:7){
    print(colnames(dsCat[i]))
    chiTestOut <- chisq.test(dsOutRemoved[,var], dsCat[,i])
    print(chiTestOut$p.value)}
}

#chisquare test with casual
var <- "casual"
chiTestFunc(dsCat,var)

#chisquare test with registered
var <- "registered"
chiTestFunc(dsCat,var)
remove(col)
corrMatTemp <- cor(dsPredictors)

#removing unwanted feature atemp due to corr analysis
dsOutRemoved$atemp <- NULL
