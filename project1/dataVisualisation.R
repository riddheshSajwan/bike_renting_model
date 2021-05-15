library(psych)
library(ggplot2)

#reading csv
ds <- read.csv("C:/Users/riddh/OneDrive/Documents/Edwisor/Project 1/day.csv", sep = ",")
#removing unwanted variables
dsPredictors <- ds[,3:16]
#renaming colnames
dsPredictors <- dplyr::rename(dsPredictors, 'year' = yr,
                      'month' = mnth, 'weatherSituation' = weathersit,
                      'count' = cnt, 'humidity' = hum)

#Probability Density Funciton with histogram and 
multi.hist(dsPredictors, main = NULL, dcol = c("blue", "red"),
           dlty = c("solid", "solid"), bcol = "linen")


#histogram with mean line
for (i in 8:14){
  
  title_val <- paste("Histogram and Mean of ",colnames(dsPredictors[i])) 
  hist_plot<-ggplot(dsPredictors, aes(x=dsPredictors[,i])) + 
    geom_histogram(binwidth=0.5,color="darkblue", fill="lightblue",
                   linetype="solid")+
    labs(title=title_val,x=colnames(dsPredictors[i]), y = "Frequency")+
    theme_classic() +
    geom_vline(aes(xintercept=mean(dsPredictors[,i])),
               color="red", linetype="dashed", size=1)
  
  print(hist_plot)
}

#outlier analysis
drawBoxP <- function(df){
  for (i in 8:14){  
    title_val <- paste("Boxplot with Outliers of ",colnames(df[i])) # paste("Boxplot without Outliers of ",colnames(df[i]))
    i<-12
    box_plot <- ggplot(df, aes(x='', y= df[,i])) + 
                geom_boxplot(outlier.color = "red", outlier.shape = 19,
                   outlier.size = 1.5, outlier.stroke = 0.5) + 
                labs(title=title_val,x=colnames(df[i]), y = "Frequency") + 
                geom_jitter(shape=16, position=position_jitter(0.2)) #with jitter
    print(box_plot)
  }  
}
#boxplot with outliers
drawBoxP(dsPredictors)
#removing outliers from windspeed
unwanted_outliers <- boxplot(dsPredictors$windspeed, plot = FALSE)$out
dsOutRemoved <- dsPredictors[-which(dsPredictors$windspeed %in% unwanted_outliers ),]
drawBoxP(dsOutRemoved)

#effect of outliers

#with outliers

boxplot(dsPredictors$windspeed, main="Boxplot for windspeed", 
        ylab="windspeed")
hist(dsPredictors$windspeed, main="Histogram for windspeed", 
     xlab="windspeed")
#without outliers
boxplot(dsOutRemoved$windspeed, main="Boxplot for windspeed", 
        ylab="windspeed")
hist(dsOutRemoved$windspeed, main="Histogram for windspeed", 
     xlab="windspeed")
